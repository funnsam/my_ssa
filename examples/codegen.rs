use my_ssa::{ssa::*, builder::*, instruction};

fn main() {
    let mut builder = Builder::default();

    let main = builder.new_function("main", ());

    let x = builder.allocate_variable();
    let y = builder.allocate_variable();
    let t = builder.allocate_variable();

    let r = builder.append_block("entry", ());
    let a = builder.append_block("A", ());
    let b = builder.append_block("B", ());
    let c = builder.append_block("C", ());
    let d = builder.append_block("D", ());
    let e = builder.append_block("E", ());
    let zero = builder.allocate_value();

    builder.set_terminator(r, Terminator::Jump(a));

    builder.set_terminator(a, Terminator::Branch(zero, b, c));

    let x_1 = builder.allocate_value();
    let y_1 = builder.allocate_value();
    builder.append_instruction(b, instruction!(Operation::Integer(0) => x_1));
    builder.append_instruction(b, instruction!(Operation::Integer(0) => y_1));
    builder.append_instruction(b, instruction!(Operation::Store(x, x_1)));
    builder.append_instruction(b, instruction!(Operation::Store(y, y_1)));
    builder.set_terminator(b, Terminator::Jump(d));

    let x_2 = builder.allocate_value();
    builder.append_instruction(c, instruction!(Operation::Load(x) => x_2));
    builder.append_instruction(c, instruction!(Operation::Store(t, x_2)));
    let y_2 = builder.allocate_value();
    builder.append_instruction(c, instruction!(Operation::Load(y) => y_2));
    builder.append_instruction(c, instruction!(Operation::Store(x, y_2)));
    let t_1 = builder.allocate_value();
    builder.append_instruction(c, instruction!(Operation::Load(t) => t_1));
    builder.append_instruction(c, instruction!(Operation::Store(y, t_1)));
    builder.set_terminator(c, Terminator::Branch(zero, d, e));

    let x_3 = builder.allocate_value();
    builder.append_instruction(d, instruction!(Operation::Load(x) => x_3));
    let y_3 = builder.allocate_value();
    builder.append_instruction(d, instruction!(Operation::Load(y) => y_3));
    let tmp = builder.allocate_value();
    builder.append_instruction(d, instruction!(Operation::BinOp(BinOp::Add, x_3, y_3) => tmp));
    builder.append_instruction(d, instruction!(Operation::Store(x, tmp)));
    builder.set_terminator(d, Terminator::Branch(zero, a, e));

    let x_4 = builder.allocate_value();
    builder.append_instruction(e, instruction!(Operation::Load(x) => x_4));
    builder.set_terminator(e, Terminator::Return(Some(x_4)));

    println!("{}", builder.get_body());
    let ssa = builder.get_ssa();
    println!("{ssa}");
}
