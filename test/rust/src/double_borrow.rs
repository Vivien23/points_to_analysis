fn main(){
    let x = 42;
    let y = &x;
    let z = &&x;
    let _test1 = **z;
    let _test2 = &*z;
    let _test3 = &&x;
    let _test4 = z;
    let _test5 = &*&*y;
}



