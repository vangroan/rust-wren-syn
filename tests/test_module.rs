use rust_wren_syn::{Lexer, Module, Parse, TokenStream};

#[test]
fn test_module_parse() {
    let lexer = Lexer::new(include_str!("test.wren"));
    let mut tokens = TokenStream::new(lexer);

    let module = Module::parse(&mut tokens);
    println!("{:#?}", module);
}
