use my_ssa::{ssa::*, builder::*, instruction};

fn main() {
    let mut builder = Builder::default();

    let main = builder.new_function("main", ());
    let b1 = builder.append_block("1", ());
    let b2 = builder.append_block("2", ());
    let b3 = builder.append_block("3", ());
    let b4 = builder.append_block("4", ());
    let b5 = builder.append_block("5", ());
    let t = builder.allocate_value();

    builder.set_terminator(b1, Terminator::Jump(b2));
    builder.set_terminator(b2, Terminator::Branch(t, b3, b4));
    builder.set_terminator(b3, Terminator::Jump(b5));
    builder.set_terminator(b4, Terminator::Jump(b5));
    builder.set_terminator(b5, Terminator::Jump(b2));

    builder.get_ssa();
}
