use my_ssa::{ssa::*, builder::*, instruction};

fn main() {
    let mut builder = Builder::default();

    let main = builder.new_function("main", ());
    let a = builder.append_block("A", ());
    let b = builder.append_block("B", ());

    let x = builder.allocate_variable();

    let zero = builder.allocate_value();
    let xt = builder.allocate_value();

    builder.append_instruction(a, instruction!(Operation::Store(x, zero)));
    builder.set_terminator(a, Terminator::Branch(zero, a, b));

    builder.append_instruction(b, instruction!(Operation::Load(x) => xt));
    builder.set_terminator(b, Terminator::Return(Some(xt)));

    println!("{}", builder.get_body());
    let ssa = builder.get_ssa();
    println!("{ssa}");
}
