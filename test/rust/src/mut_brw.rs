fn main(){
    let mut x = 42;
    let y = &mut x;
    *y = 4;
}