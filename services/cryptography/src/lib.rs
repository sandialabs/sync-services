use crystals_dilithium::sign::lvl2::*;
use crystals_dilithium::dilithium2::*;

use hex::encode;
use hex::decode;

pub fn mldsa2_keygen(seed: Option<&str>) -> (String, String) {
    let mut pkb: [u8; PUBLICKEYBYTES] = [42; 1312];
    let mut skb: [u8; SECRETKEYBYTES] = [42; 2528];
    if seed == None{
        keypair(&mut pkb, &mut skb, None);
    } else {
        let seed_bytes: [u8; 32] = decode(seed.unwrap()).unwrap().try_into().expect("seed not of correct length");
        println!("seed: {}", encode(seed_bytes));
        keypair(&mut pkb, &mut skb, Some(&seed_bytes));
    } 
    let pks = encode(pkb);
    let sks = encode(skb);
    return (pks, sks)
}

pub fn mldsa2_sign(sks: String, msg: &str) -> String{
    let mut sigb: [u8; SIGNBYTES] = [42; 2420];
    let skb = decode(sks).unwrap();
    let msgb = msg.as_bytes();
    signature(&mut sigb, msgb, &skb, false);
    return encode(sigb);
}

pub fn mldsa2_verify(pks: String, msg: &str, sigs: String) -> Result<(), Box<dyn std::error::Error>> {
    let pkb = decode(pks).unwrap();
    let msgb = msg.as_bytes();
    let sigb = decode(sigs).unwrap();

    if verify(&sigb, msgb, &pkb) {
        Ok(())
    } 
    else {
        Err("Signature verification failed")?
    }
}
pub fn bmldsa2_keygen(seed: Option<Vec<u8>>) -> ([u8; 1312], [u8; 2528]) {
    let mut pkb: [u8; PUBLICKEYBYTES] = [42; 1312];
    let mut skb: [u8; SECRETKEYBYTES] = [42; 2528];
    if seed == None{
        keypair(&mut pkb, &mut skb, None);
    } else {
        keypair(&mut pkb, &mut skb, seed.as_deref());
    } 
    return (pkb, skb)
}

pub fn bmldsa2_sign(skb: Vec<u8>, msgb: &[u8]) -> [u8; 2420]{
    let mut sigb: [u8; SIGNBYTES] = [42; 2420];
    signature(&mut sigb, msgb, &skb, false);
    return sigb;
}

pub fn bmldsa2_verify(pkb: Vec<u8>, msgb: &[u8], sigb: Vec<u8>) -> Result<(), Box<dyn std::error::Error>> {
    if verify(&sigb, msgb, &pkb) {
        Ok(())
    } 
    else {
        Err("Signature verification failed")?
    }
}
