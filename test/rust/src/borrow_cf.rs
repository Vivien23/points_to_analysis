fn secret() -> bool{
    true
}

fn main(){
    let mut x = 0;
    let y = &mut x;
    if secret {
        let z = &mut x;
    }

}