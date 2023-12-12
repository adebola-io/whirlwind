mod terminal;

fn main() {
    let args: Vec<_> = std::env::args().collect();
    println!("{:?}", args);
}
