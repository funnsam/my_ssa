use std::fmt::{self, Display, Formatter};
use std::collections::*;

macro_rules! type_wrapper {
    ($tv: vis $type: ident = $iv: vis $inner: ty : $fmt: tt) => {
        #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
        $tv struct $type($iv $inner);

        impl ::std::ops::Deref for $type {
            type Target = $inner;

            fn deref(&self) -> &$inner {
                &self.0
            }
        }

        impl ::std::ops::DerefMut for $type {
            fn deref_mut(&mut self) -> &mut $inner {
                &mut self.0
            }
        }

        impl ::std::fmt::Display for $type {
            fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                ::std::write!(fmt, $fmt, self.0)
            }
        }
    };
}

macro_rules! binop {
    ($($name: ident = $op: tt),* $(,)?) => {
        pub enum BinOp {
            $($name),*
        }

        impl ::std::fmt::Display for BinOp {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    $(BinOp::$name => write!(f, "{}", stringify!($name).to_lowercase())),*
                }
            }
        }

        impl BinOp {
            pub fn operate(&self, lhs: u64, rhs: u64) -> u64 {
                match self {
                    $(BinOp::$name => (lhs $op rhs) as u64),*
                }
            }
        }
    };
}

type_wrapper!(pub ValueId = pub(crate) usize    : "%{}");
type_wrapper!(pub VariableId = pub(crate) usize : "#{}");
type_wrapper!(pub BlockId = pub(crate) usize    : "${}");
type_wrapper!(pub FunctionId = pub(crate) usize    : "@{}");

#[derive(Default)]
pub struct Body<Debug> {
    pub blocks: Vec<Block<Debug>>,
}

pub struct Block<Debug> {
    pub name: String,
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
    pub debug: Debug,
}

pub struct Instruction {
    pub destination: Option<ValueId>,
    pub operation: Operation,
}

pub enum Operation {
    Integer(u64),
    BinOp(BinOp, ValueId, ValueId),
    Call(FunctionId, Vec<ValueId>),
    Allocate(VariableId, ValueId),
    Load(VariableId),
    Store(VariableId, ValueId),
    Phi(Vec<(ValueId, BlockId)>),
}

pub struct Function<Debug> {
    pub(crate) body: Body<Debug>,
    pub(crate) name: String,
    pub(crate) debug: Debug,
}

binop!(
    Add = +,
    Sub = -,
    Mul = *,
    Div = /,
    Mod = %,
    And = &,
    Or  = |,
    Xor = ^,
    Shl = <<,
    Shr = >>,
    Eq  = ==,
    Ne  = !=,
    Lt  = <,
    Le  = <=,
    Gt  = >,
    Ge  = >=,
);

pub enum Terminator {
    Return(Option<ValueId>),
    Jump(BlockId),
    Branch(ValueId, BlockId, BlockId),
    None,
}

impl<Debug> Body<Debug> {
    pub(crate) fn ssa_form(&mut self, alloc: &mut crate::builder::ResourceAllocator) {
        self.insert_phi(alloc);

        for b in self.blocks.iter_mut() {
            b.remove_load_store();
        }
    }

    fn insert_phi(&mut self, alloc: &mut crate::builder::ResourceAllocator) {
        let mut def: HashMap<VariableId, HashMap<BlockId, ValueId>> = HashMap::new();

        for (i, bb) in self.blocks.iter().enumerate() {
            for inst in bb.instructions.iter() {
                match inst.operation {
                    Operation::Store(var, val) => if let Some(w) = def.get_mut(&var) {
                        w.insert(BlockId(i), val);
                    } else {
                        let mut a = HashMap::new();
                        a.insert(BlockId(i), val);
                        def.insert(var, a);
                    },
                    _ => {},
                }
            }
        }

        let mut w = def.clone();

        let mut f = vec![HashSet::new(); self.blocks.len()];

        for (v, w) in w.iter_mut() {
            while let Some((bbi, val)) = w.iter().next() {
                let bbi = *bbi;
                let val = *val;

                w.remove(&bbi);

                for y in self.df(bbi) {
                    if !f[*bbi].contains(&y) {
                        let phi = if f.iter().position(|a| a.contains(&y)).is_none() {
                            let phi = alloc.allocate_value();

                            self.blocks[*y].instructions.insert(0, crate::instruction!(Operation::Phi(vec![(val, bbi)]) => phi));
                            self.blocks[*y].replace_variable(*v, phi);

                            phi
                        } else {
                            match &mut self.blocks[*y].instructions[0] {
                                Instruction { destination, operation: Operation::Phi(l) } => {
                                    l.push((val, bbi));
                                    destination.unwrap()
                                },
                                _ => unreachable!(),
                            }
                        };

                        f[*bbi].insert(y);

                        if !def.get(v).unwrap().contains_key(&y) {
                            w.insert(y, phi);
                        }
                    }
                }
            }

            for f in f.iter_mut() {
                f.clear();
            }
        }
    }

    fn df(&self, x: BlockId) -> Vec<BlockId> {
        let mut df = Vec::new();

        for (wi, _) in self.blocks.iter().enumerate() {
            if !self.is_strict_dom(x, BlockId(wi)) && self.predecessors(BlockId(wi)).iter().filter(|pred| self.is_dominance(x, **pred)).count() != 0 {
                df.push(BlockId(wi));
            }
        }

        df
    }

    fn is_dominance(&self, f: BlockId, t: BlockId) -> bool {
        if f == t {
            return true;
        }

        let mut visited = HashSet::new();
        self._is_dominance(BlockId(0), f, t, &mut visited, false) == 1
    }

    fn _is_dominance(&self, at: BlockId, f: BlockId, t: BlockId, visited: &mut HashSet<BlockId>, ok: bool) -> usize {
        let ok = ok || at == f;

        if at == t {
            return ok as usize;
        } else if visited.contains(&at) {
            return 2;
        }

        visited.insert(at);

        let ret = match self.blocks[*at].terminator {
            Terminator::Jump(a) => self._is_dominance(a, f, t, visited, ok),
            Terminator::Branch(_, a, b) => {
                let a = self._is_dominance(a, f, t, visited, ok);
                let b = self._is_dominance(b, f, t, visited, ok);

                if a == 0 || b == 0 {
                    0
                } else if a == 2 && b == 2 {
                    2
                } else {
                    1
                }
            },
            _ => 2,
        };

        if ret == 1 {
            visited.remove(&at);
        }

        ret
    }

    fn is_strict_dom(&self, f: BlockId, t: BlockId) -> bool {
        f != t && self.is_dominance(f, t)
    }

    fn predecessors(&self, bbi: BlockId) -> Vec<BlockId> {
        self.blocks.iter().enumerate().filter(|(_, b)| match b.terminator {
            Terminator::Jump(i) => i == bbi,
            Terminator::Branch(_, a, b) => a == bbi || b == bbi,
            _ => false,
        }).map(|(i, _)| BlockId(i)).collect()
    }
}

impl<Debug> Block<Debug> {
    fn replace_variable(&mut self, var: VariableId, mut val: ValueId) {
        let mut replace = HashMap::new();

        for i in self.instructions.iter_mut() {
            match i {
                Instruction { destination: Some(d), operation: Operation::Load(lvar) } if *lvar == var => {
                    replace.insert(*d, val);
                },
                Instruction { operation: Operation::Store(svar, sval), .. } if *svar == var => {
                    val = *sval;
                },
                Instruction { operation: Operation::BinOp(_, a, b), .. } => {
                    replace.get(&a).map(|n| *a = *n);
                    replace.get(&b).map(|n| *b = *n);
                },
                Instruction { operation: Operation::Call(_, v), .. } => {
                    for v in v.iter_mut() {
                        replace.get(&v).map(|n| *v = *n);
                    }
                },
                Instruction { operation: Operation::Integer(..) | Operation::Allocate(..) | Operation::Phi(..) | Operation::Load(..) | Operation::Store(..), .. } => {},
            }
        }

        match &mut self.terminator {
            Terminator::Branch(v, _, _) => {
                replace.get(&v).map(|n| *v = *n);
            },
            Terminator::Return(v) => {
                v.as_mut().and_then(|v| replace.get(&v).map(|n| *v = *n));
            },
            _ => {},
        }
    }

    fn remove_load_store(&mut self) {
        let mut remove = Vec::new();

        for (i, e) in self.instructions.iter().enumerate() {
            match e.operation {
                Operation::Load(..) | Operation::Store(..) => remove.push(i),
                _ => {},
            }
        }

        for (i, e) in remove.into_iter().enumerate() {
            self.instructions.remove(e - i);
        }
    }
}

impl<Debug: std::fmt::Debug> Display for Body<Debug> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        for blk in self.blocks.iter() {
            write!(fmt, "{blk}")?;
        }

        Ok(())
    }
}

impl<Debug: std::fmt::Debug> Display for Block<Debug> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        writeln!(fmt, "\x1b[35m${}\x1b[0m: \x1b[90m// {} {:?}\x1b[0m", *self.id, self.name, self.debug)?;

        for instr in &self.instructions {
            writeln!(fmt, "    {}", instr)?;
        }

        writeln!(fmt, "    {}\x1b[0m", self.terminator)?;

        Ok(())
    }
}

impl Display for Terminator {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            Self::Return(Some(v))           => write!(fmt, "\x1b[32mret \x1b[36m{v}"),
            Self::Return(None)              => write!(fmt, "\x1b[32mret"),
            Self::Jump(block)               => write!(fmt, "\x1b[32mjmp \x1b[35m{block}"),
            Self::Branch(cond, if_, else_)  => write!(fmt, "\x1b[32mbr \x1b[36m{cond} \x1b[35m{if_} {else_}"),
            Self::None                      => write!(fmt, "\x1b[1;31mno terminator!"),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        if let Some(dst) = self.destination {
            write!(fmt, "\x1b[36m{dst}\x1b[0m = ")?;
        }

        write!(fmt, "{}\x1b[0m", self.operation)
    }
}

impl Display for Operation {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            Self::Integer(int)      => write!(fmt, "{int}"),
            Self::BinOp(op, l, r)   => write!(fmt, "\x1b[33m{op} \x1b[36m{l} {r}"),
            Self::Call(f, args)     => write!(fmt, "\x1b[33mcall \x1b[34m{f}\x1b[0m({})", args.iter()
                .map(|e| format!("\x1b[36m{}\x1b[0m", e))
                .collect::<Vec<String>>()
                .join(", ")
            ),
            Self::Allocate(v, size) => write!(fmt, "\x1b[33malloc \x1b[34m{v} \x1b[36m{size}"),
            Self::Load(var)         => write!(fmt, "\x1b[33mload \x1b[34m{var}"),
            Self::Store(v, val)     => write!(fmt, "\x1b[33mstore \x1b[34m{v} \x1b[36m{val}"),
            Self::Phi(branches)     => write!(fmt, "\x1b[33mphi \x1b[34m{}", branches.iter()
                .map(|(val, block)| format!("{}: {}", block, val))
                .collect::<Vec<String>>()
                .join(", ")
            ),
        }
    }
}

#[macro_export]
macro_rules! instruction {
    ($oper: expr => $dest: expr) => {{
        $crate::ssa::Instruction { operation: $oper, destination: Some($dest) }
    }};
    ($oper: expr) => {{
        $crate::ssa::Instruction { operation: $oper, destination: None }
    }};
}

#[derive(Default)]
pub struct Ssa<Debug> {
    pub(crate) functions: Vec<Function<Debug>>,
}

impl<Debug: std::fmt::Debug> Display for Ssa<Debug> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        for (i, f) in self.functions.iter().enumerate() {
            writeln!(fmt, "\x1b[1;4m{}:\x1b[0m", f.name)?;
            write!(fmt, "{}", f.body)?;
            if i != self.functions.len() - 1 {
                writeln!(fmt)?;
            }
        }

        Ok(())
    }
}
