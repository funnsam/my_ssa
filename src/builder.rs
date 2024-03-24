use super::ssa::*;

#[derive(Default)]
pub struct Builder<Debug> {
    functions: Ssa<Debug>,

    alloc: ResourceAllocator,
    function_id: FunctionId,
    block_id: BlockId,
}

impl<Debug: Default> Builder<Debug> {
    pub fn get_ssa(mut self) -> Ssa<Debug> {
        for f in self.functions.functions.iter_mut() {
            f.body.ssa_form(&mut self.alloc);
        }
        self.functions
    }

    pub fn new_function<A: Into<String>>(&mut self, name: A, debug: Debug) -> FunctionId {
        self.functions.functions.push(Function {
            body: Body::default(),
            name: name.into(),
            debug
        });
        *self.function_id += 1;
        FunctionId(*self.function_id - 1)
    }

    pub fn append_block<A: Into<String>>(&mut self, name: A, debug: Debug) -> BlockId {
        let block_id = self.block_id;
        self.get_body_mut().blocks.push(Block {
            name: name.into(),
            id: block_id,
            instructions: Vec::new(),
            terminator: Terminator::None,
            debug
        });
        *self.block_id += 1;
        BlockId(*self.block_id - 1)
    }

    pub fn append_instruction(&mut self, block: BlockId, inst: Instruction) {
        self.get_body_mut().blocks[*block].instructions.push(inst)
    }

    pub fn set_terminator(&mut self, block: BlockId, term: Terminator) {
        self.get_body_mut().blocks[*block].terminator = term;
    }

    pub fn allocate_value(&mut self) -> ValueId {
        self.alloc.allocate_value()
        // *self.value_id += 1;
        // ValueId(*self.value_id - 1)
    }

    pub fn allocate_variable(&mut self) -> VariableId {
        self.alloc.allocate_variable()
        // *self.variable_id += 1;
        // VariableId(*self.variable_id - 1)
    }

    pub fn get_body<'a>(&'a self) -> &'a Body<Debug> {
        &self.functions.functions[*self.function_id - 1].body
    }

    fn get_body_mut<'a>(&'a mut self) -> &'a mut Body<Debug> {
        &mut self.functions.functions[*self.function_id - 1].body
    }
}

#[derive(Default)]
pub struct ResourceAllocator {
    variable_id: VariableId,
    value_id: ValueId,
}

impl ResourceAllocator {
    pub fn allocate_value(&mut self) -> ValueId {
        *self.value_id += 1;
        ValueId(*self.value_id - 1)
    }

    pub fn allocate_variable(&mut self) -> VariableId {
        *self.variable_id += 1;
        VariableId(*self.variable_id - 1)
    }
}
