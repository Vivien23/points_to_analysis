// This code has UB because of a use after free
// It is based on the issue RUSTSEC-2025-0004
// Original code at https://github.com/sfackler/rust-openssl/security/advisories/GHSA-rpmj-rpgj-qmpm

fn main() {
    // builder.set_alpn_select_callback(|_, client_protos| {
    //    let server_protos = b"\x02h2".to_vec();
    //    ssl::select_next_proto(&server_protos, client_protos).ok_or_else(AlpnError::NOACK)
    // });
}
