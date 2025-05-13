// This code has UB because of a dangling reference
// It is based on the issue RUSTSEC-2020-0091 
// Original code at https://github.com/vorner/arc-swap/issues/45

fn main() {
    return ()
}