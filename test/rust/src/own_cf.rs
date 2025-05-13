fn secret() -> bool{
    true
}

fn main(){
    let x = 0;
    let _y = x;
    if secret() {
        let _z = x;
    }

}