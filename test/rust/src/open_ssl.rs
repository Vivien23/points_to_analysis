fn main() {
    builder.set_alpn_select_callback(|_, client_protos| {
        let server_protos = b"\x02h2".to_vec();
        ssl::select_next_proto(&server_protos, client_protos).ok_or_else(AlpnError::NOACK)
    });
}
