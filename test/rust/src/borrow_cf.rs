fn secret() -> bool{
    true
}

fn main(){
    let mut x = 0;
    let _y = &mut x;
    if secret() {
        let _z = &mut x;
    }

}