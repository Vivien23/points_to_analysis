fn main() {
    let mut x = 0;
    let y = &mut x;
    let y1 = y as *mut _;
    let y2 = y as *mut _;
    unsafe{*y1 = 1; *y2 = 2;};
    println!("{x}")
}
