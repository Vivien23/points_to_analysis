fn main() {
    let mut x = String::from("Helllo");
    let y = &mut x;
    let y = y as *mut _;
    let z = x; // z has now ownership of "Hello", and x should be freed
    unsafe{ *y = String::from("World")}
    println!("{z}");
}