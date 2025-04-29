fn main() {
    let x = 0;
    // If we do the following, it doesn't compile with error "assigning to `&T` is undefined behavior"
    //let y: *const String = & x;
    //let y = y as *mut _;
    //unsafe{*y = String::from("World")};

    // But it's okay if we hide it in a function
    ub (&x);
    println!("{x}");
}

fn ub(arg :*const u8) {
    let arg = arg as *mut _;
    unsafe{*arg = 1};
}