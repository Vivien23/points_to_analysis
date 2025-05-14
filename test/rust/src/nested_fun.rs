fn main() {
    let var = 42;

    fn nested(x: u8) -> u8 {
        x
    }

    let _y = nested(var);
}
