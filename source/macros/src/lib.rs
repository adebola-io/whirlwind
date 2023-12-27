use proc_macro::TokenStream;

extern crate proc_macro;
extern crate quote;
extern crate syn;

#[proc_macro_derive(Signature)]
pub fn signature(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident;

    let expanded = quote::quote! {
        impl Signature for #name {
            fn name(&self) -> &str {
                self.name.name.as_str()
            }
            fn info(&self) -> Option<&Vec<String>> {
                self.info.as_ref()
            }
            fn is_public(&self) -> bool {
                self.is_public
            }
        }
    };

    TokenStream::from(expanded)
}
