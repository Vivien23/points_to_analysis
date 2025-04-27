fn main() {
    let mut x = 0;
    let y = &mut x;
    let y = y as *mut _;
    let z = &mut x;
    unsafe{*y = 1;}
    *z = 0;
}