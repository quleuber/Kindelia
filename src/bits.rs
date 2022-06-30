pub use bit_vec::BitVec;

use crate::crypto;
use crate::hvm::*;
use crate::node::*;
use crate::util::*;

use primitive_types::U256;

// Serializers
// ===========

// A number with a known amount of bits

pub fn serialize_fixlen(size: u128, value: &U256, bits: &mut BitVec) {
  for i in 0 .. size {
    bits.push((value >> i).low_u128() & 1 == 1);
  }
}

pub fn deserialize_fixlen(size: u128, bits: &BitVec, index: &mut u128) -> Option<U256> {
  let mut result = u256(0);
  for i in 0 .. size {
    let index = (*index + size - i - 1) as usize;
    if index >= bits.len() {
      return None;
    }
    result = result * u256(2) + u256(bits[index] as u128); 

  }
  *index = *index + size;
  Some(result)
}

// A number with an unknown amount of bits

pub fn serialize_varlen(value: &U256, bits: &mut BitVec) {
  let mut value : U256 = *value;
  while value > u256(0) {
    bits.push(true);
    bits.push(value.low_u128() & 1 == 1);
    value = value >> u256(1);
  }
  bits.push(false);
}

pub fn deserialize_varlen(bits: &BitVec, index: &mut u128) -> Option<U256> {
  let mut val : U256 = u256(0);
  let mut add : U256 = u256(1);
  while bits.get(*index as usize)? {
    val = val + if bits.get(*index as usize + 1)? { add } else { u256(0) };
    add = add.saturating_mul(u256(2));
    *index = *index + 2;
  }
  *index = *index + 1;
  return Some(val);
}

// A number

pub fn serialize_number(value: &U256, bits: &mut BitVec) {
  let size = value.bits() as u128;
  serialize_varlen(&u256(size), bits);
  serialize_fixlen(size, value, bits);
}

pub fn deserialize_number(bits: &BitVec, index: &mut u128) -> Option<U256> {
  let size = deserialize_varlen(&bits, index)?.low_u128();
  let numb = deserialize_fixlen(size, &bits, index)?;
  return Some(numb);
}

// A bitvec with an unknown amount of bits

pub fn serialize_bits(data: &BitVec, bits: &mut BitVec) {
  for bit in data.iter() {
    bits.push(true);
    bits.push(bit);
  }
  bits.push(false);
}

pub fn deserialize_bits(bits: &BitVec, index: &mut u128) -> Option<BitVec> {
  let mut result = BitVec::new();
  while bits.get(*index as usize)? {
    result.push(bits.get(*index as usize + 1)?);
    *index = *index + 2;
  }
  *index = *index + 1;
  return Some(result);
}

// A name is grouped by 6-bit letters

pub fn serialize_name(name: &u128, bits: &mut BitVec) {
  let mut name = *name;
  bits.push(false); // compressed-name flag
  while name > 0 {
    bits.push(true);
    serialize_fixlen(6, &u256(name & 0x3F), bits);
    name = name >> 6;
  }
  bits.push(false);
}

pub fn deserialize_name(bits: &BitVec, index: &mut u128) -> Option<u128> {
  let mut nam : u128 = 0;
  let mut add : u128 = 1;
  *index += 1; // ignore the compressed-name flag
  while bits.get(*index as usize)? {
    *index += 1;
    let got = deserialize_fixlen(6, bits, index)?.low_u128();
    nam = nam + add * got;
    add = add.saturating_mul(64);
  }
  *index = *index + 1;
  return if add > 1 { Some(nam) } else { None };
}


// Many elements, unknown length

pub fn serialize_list<T>(serialize_one: impl Fn(&T, &mut BitVec) -> (), values: &[T], bits: &mut BitVec) {
  for x in values {
    bits.push(true);
    serialize_one(x, bits);
  }
  bits.push(false);
}

pub fn deserialize_list<T>(deserialize_one: impl Fn(&BitVec, &mut u128) -> Option<T>, bits: &BitVec, index: &mut u128) -> Option<Vec<T>> {
  let mut result = Vec::new();
  while bits.get(*index as usize)? {
    *index = *index + 1;
    result.push(deserialize_one(bits, index)?);
  }
  *index = *index + 1;
  Some(result)
}

// Many elements, known length

pub fn serialize_vector<T>(serialize_one: impl Fn(&T, &mut BitVec) -> (), size: u128, data: &[T], bits: &mut BitVec) {
  if data.len() as u128 != size {
    panic!("Incorrect serialization vector size.");
  }
  for x in data {
    serialize_one(x, bits);
  }
}

pub fn deserialize_vector<T>(deserialize_one: impl Fn(&BitVec, &mut u128) -> Option<T>, size: u128, bits: &BitVec, index: &mut u128) -> Option<Vec<T>> {
  let mut result = Vec::new();
  for _ in 0 .. size {
    result.push(deserialize_one(bits, index)?);
  }
  Some(result)
}

// An address

pub fn serialize_address(address: &Address, bits: &mut BitVec) {
  match address {
    Address::IPv4 { val0, val1, val2, val3, port } => {
      bits.push(false);
      serialize_fixlen(8, &u256(*val0 as u128), bits);
      serialize_fixlen(8, &u256(*val1 as u128), bits);
      serialize_fixlen(8, &u256(*val2 as u128), bits);
      serialize_fixlen(8, &u256(*val3 as u128), bits);
      serialize_fixlen(16, &u256(*port as u128), bits);
    }
  }
}

pub fn deserialize_address(bits: &BitVec, index: &mut u128) -> Option<Address> {
  if bits[*index as usize] as u128 == 0 {
    *index = *index + 1;
    let val0 = deserialize_fixlen(8, bits, index)?.low_u128() as u8;
    let val1 = deserialize_fixlen(8, bits, index)?.low_u128() as u8;
    let val2 = deserialize_fixlen(8, bits, index)?.low_u128() as u8;
    let val3 = deserialize_fixlen(8, bits, index)?.low_u128() as u8;
    let port = deserialize_fixlen(16, bits, index)?.low_u128() as u16;
    return Some(Address::IPv4 { val0, val1, val2, val3, port });
  } else {
    return None;
  }
}

pub fn serialized_address(address: &Address) -> BitVec {
  let mut bits = BitVec::new();
  serialize_address(address, &mut bits);
  return bits;
}

pub fn deserialized_address(bits: &BitVec) -> Option<Address> {
  deserialize_address(bits, &mut 0)
}

// A peer

pub fn serialize_peer(peer: &Peer, bits: &mut BitVec) {
  serialize_address(&peer.address, bits);
  serialize_fixlen(48, &u256(peer.seen_at as u128), bits);
}

pub fn deserialize_peer(bits: &BitVec, index: &mut u128) -> Option<Peer> {
  let address = deserialize_address(bits, index)?;
  let seen_at = deserialize_fixlen(48, bits, index)?.low_u128();
  return Some(Peer { address, seen_at });
}

pub fn serialized_peer(peer: &Peer) -> BitVec {
  let mut bits = BitVec::new();
  serialize_peer(peer, &mut bits);
  return bits;
}

pub fn deserialized_peer(bits: &BitVec) -> Option<Peer> {
  deserialize_peer(bits, &mut 0)
}

// A block

pub fn serialize_block(block: &Block, bits: &mut BitVec) {
  serialize_fixlen(256, &block.prev, bits);
  serialize_fixlen(128, &u256(block.time), bits);
  serialize_fixlen(128, &u256(block.rand), bits);
  // TODO: optimize
  serialize_bytes(BODY_SIZE as u128, &block.body.value, bits);
}

pub fn deserialize_block(bits: &BitVec, index: &mut u128) -> Option<Block> {
  let prev = deserialize_fixlen(256, bits, index)?;
  let time = deserialize_fixlen(128, bits, index)?.low_u128();
  let rand = deserialize_fixlen(128, bits, index)?.low_u128();
  let body = deserialize_bytes(BODY_SIZE as u128, bits, index)?;
  let mut value : [u8; BODY_SIZE] = [0; BODY_SIZE];
  value[..BODY_SIZE].copy_from_slice(&body[..BODY_SIZE]);
  return Some(new_block(prev, time, rand, Body { value }));
}

pub fn serialized_block(block: &Block) -> BitVec {
  let mut bits = BitVec::new();
  serialize_block(block, &mut bits);
  return bits;
}

pub fn deserialized_block(bits: &BitVec) -> Option<Block> {
  deserialize_block(bits, &mut 0)
}

// A hash

pub fn serialize_hash(hash: &Hash, bits: &mut BitVec) {
  serialize_fixlen(256, hash, bits);
}

pub fn deserialize_hash(bits: &BitVec, index: &mut u128) -> Option<Hash> {
  deserialize_fixlen(256, bits, index)
}

// Bytes

pub fn serialize_bytes(size: u128, bytes: &[u8], bits: &mut BitVec) {
  if size as usize != bytes.len() {
    panic!("Incorrect serialize_bytes size.");
  }
  for byte in bytes {
    serialize_fixlen(8, &u256(*byte as u128), bits);
  }
}

pub fn deserialize_bytes(size: u128, bits: &BitVec, index: &mut u128) -> Option<Vec<u8>> {
  let mut result = Vec::new();
  for _ in 0 .. size {
    result.push(deserialize_fixlen(8, bits, index)?.low_u128() as u8);
  }
  Some(result)
}

// A message

pub fn serialize_message(message: &Message, bits: &mut BitVec) {
  match message {
    Message::NoticeThisBlock { block, istip, peers } => {
      // tag   : 4 bits
      // block : (256 + 128 + 128 + 10240) bits
      // istip : 1 bit
      // peers : (1 + (1 + 1 + 8 + 8 + 8 + 8 + 16 + 48) * len) bits
      // total : (4 + 256 + 128 + 128 + 10240 + 1 + 1 + (1 + 1 + 8 + 8 + 8 + 8 + 16 + 48) * len) bits
      serialize_fixlen(4, &u256(0), bits);
      serialize_block(block, bits);
      serialize_fixlen(1, &(if *istip { u256(1) } else { u256(0) }), bits);
      serialize_list(serialize_peer, peers, bits);
    }
    Message::GiveMeThatBlock { bhash } => {
      serialize_fixlen(4, &u256(1), bits);
      serialize_hash(bhash, bits);
    }
    Message::PleaseMineThisTransaction { trans } => {
      if trans.data.len() == 0 {
        panic!("Invalid transaction length.");
      } else {
        serialize_fixlen(4, &u256(2), bits);
        serialize_fixlen(8, &u256(trans.len_byte() as u128), bits);
        serialize_bytes(trans.data.len() as u128, &trans.data, bits);
      }
    }
  }
}

pub fn deserialize_message(bits: &BitVec, index: &mut u128) -> Option<Message> {
  let code = deserialize_fixlen(4, bits, index)?.low_u128();
  match code {
    0 => {
      let block = deserialize_block(bits, index)?;
      let istip = deserialize_fixlen(1, bits, index)?.low_u128() != 0;
      let peers = deserialize_list(deserialize_peer, bits, index)?;
      Some(Message::NoticeThisBlock { block, istip, peers })
    }
    1 => {
      let bhash = deserialize_hash(bits, index)?;
      Some(Message::GiveMeThatBlock { bhash })
    }
    2 => {
      let size = (deserialize_fixlen(8, bits, index)?.low_u128() + 1) * 5;
      let data = deserialize_bytes(size, bits, index)?;
      Some(Message::PleaseMineThisTransaction { trans: Transaction::new(data) })
    }
    _ => None
  }
}

pub fn serialized_message(message: &Message) -> BitVec {
  let mut bits = BitVec::new();
  serialize_message(message, &mut bits);
  return bits;
}

pub fn deserialized_message(bits: &BitVec) -> Option<Message> {
  deserialize_message(bits, &mut 0)
}

// A Term

// TODO: avoid recursion here; important for checksum functionality
pub fn serialize_term(term: &Term, bits: &mut BitVec) {
  match term {
    Term::Var { name } => {
      serialize_fixlen(3, &u256(0), bits);
      serialize_name(name, bits);
    }
    Term::Dup { nam0, nam1, expr, body } => {
      serialize_fixlen(3, &u256(1), bits);
      serialize_name(nam0, bits);
      serialize_name(nam1, bits);
      serialize_term(expr, bits);
      serialize_term(body, bits);
    }
    Term::Lam { name, body } => {
      serialize_fixlen(3, &u256(2), bits);
      serialize_name(name, bits);
      serialize_term(body, bits);
    }
    Term::App { func, argm } => {
      serialize_fixlen(3, &u256(3), bits);
      serialize_term(func, bits);
      serialize_term(argm, bits);
    }
    Term::Ctr { name, args } => {
      serialize_fixlen(3, &u256(4), bits);
      serialize_name(name, bits);
      serialize_list(serialize_term, args, bits);
    }
    Term::Fun { name, args } => {
      serialize_fixlen(3, &u256(5), bits);
      serialize_name(name, bits);
      serialize_list(serialize_term, args, bits);
    }
    Term::Num { numb } => {
      serialize_fixlen(3, &u256(6), bits);
      serialize_number(&u256(*numb), bits);
    }
    Term::Op2 { oper, val0, val1 } => {
      serialize_fixlen(3, &u256(7), bits);
      serialize_fixlen(4, &u256(*oper as u128), bits);
      serialize_term(val0, bits);
      serialize_term(val1, bits);
    }
  }
}

pub fn deserialize_term(bits: &BitVec, index: &mut u128) -> Option<Term> {
  let tag = deserialize_fixlen(3, bits, index)?;
  //println!("- tag.: {} {:?}", tag, bits.clone().split_off(*index as usize));
  match tag.low_u128() {
    0 => {
      let name = deserialize_name(bits, index)?;
      Some(Term::Var { name })
    }
    1 => {
      let nam0 = deserialize_name(bits, index)?;
      let nam1 = deserialize_name(bits, index)?;
      let expr = Box::new(deserialize_term(bits, index)?);
      let body = Box::new(deserialize_term(bits, index)?);
      Some(Term::Dup { nam0, nam1, expr, body })
    }
    2 => {
      let name = deserialize_name(bits, index)?;
      let body = Box::new(deserialize_term(bits, index)?);
      Some(Term::Lam { name, body })
    }
    3 => {
      let func = Box::new(deserialize_term(bits, index)?);
      let argm = Box::new(deserialize_term(bits, index)?);
      Some(Term::App { func, argm })
    }
    4 => {
      let name = deserialize_name(bits, index)?;
      let args = deserialize_list(|bits, index| {
        let term = deserialize_term(bits, index)?;
        return Some(term);
      }, bits, index)?;
      Some(Term::Ctr { name, args })
    }
    5 => {
      let name = deserialize_name(bits, index)?;
      let args = deserialize_list(deserialize_term, bits, index)?;
      Some(Term::Fun { name, args })
    }
    6 => {
      let numb = deserialize_number(bits, index)?.low_u128();
      Some(Term::Num { numb })
    }
    7 => {
      let oper = deserialize_fixlen(4, bits, index)?.low_u128();
      let val0 = Box::new(deserialize_term(bits, index)?);
      let val1 = Box::new(deserialize_term(bits, index)?);
      Some(Term::Op2 { oper, val0, val1 })
    }
    _ => {
      None
    }
  }
}

pub fn serialized_term(term: &Term) -> BitVec {
  let mut bits = BitVec::new();
  serialize_term(term, &mut bits);
  return bits;
}

pub fn deserialized_term(bits: &BitVec) -> Option<Term> {
  deserialize_term(bits, &mut 0)
}

// A Rule

pub fn serialize_rule(rule: &Rule, bits: &mut BitVec) {
  serialize_term(&rule.lhs, bits);
  serialize_term(&rule.rhs, bits);
}

pub fn deserialize_rule(bits: &BitVec, index: &mut u128) -> Option<Rule> {
  let lhs  = deserialize_term(bits, index)?;
  let rhs  = deserialize_term(bits, index)?;
  Some(Rule{lhs, rhs})
}

// A Func

pub fn serialize_func(func: &Func, bits: &mut BitVec) {
  serialize_list(serialize_rule, &func.rules, bits);
}

pub fn deserialize_func(bits: &BitVec, index: &mut u128) -> Option<Func> {
  let rules = deserialize_list(deserialize_rule, bits, index)?;
  Some(Func { rules })
}

pub fn serialized_func(func: &Func) -> BitVec {
  let mut bits = BitVec::new();
  serialize_func(func, &mut bits);
  return bits;
}

pub fn deserialized_func(bits: &BitVec) -> Option<Func> {
  deserialize_func(bits, &mut 0)
}

// A signature

pub fn serialize_sign(sign: &Option<crypto::Signature>, bits: &mut BitVec) {
  if let Some(sign) = sign {
    serialize_fixlen(1, &u256(1), bits);
    serialize_bytes(65, &sign.0, bits);
  } else {
    serialize_fixlen(1, &u256(0), bits);
  }
}

// The double Option layer keeps it consistent, since the returned value IS an Option
pub fn deserialize_sign(bits: &BitVec, index: &mut u128) -> Option<Option<crypto::Signature>> {
  match deserialize_fixlen(1, bits, index)?.low_u128() {
    1 => {
      let data : Option<[u8; 65]> = deserialize_bytes(65, bits, index)?.try_into().ok();
      if let Some(data) = data {
        Some(Some(crypto::Signature(data)))
      } else {
        None
      }
    }
    _ => Some(None)
  }
}

// A Statement

pub fn serialize_statement(statement: &Statement, bits: &mut BitVec) {
  match statement {
    Statement::Fun { name, args, func, init, sign } => {
      serialize_fixlen(4, &u256(0), bits);
      serialize_name(name, bits);
      serialize_list(serialize_name, args, bits);
      serialize_func(func, bits);
      serialize_term(init, bits);
      serialize_sign(sign, bits);
    }
    Statement::Ctr { name, args, sign } => {
      serialize_fixlen(4, &u256(1), bits);
      serialize_name(name, bits);
      serialize_list(serialize_name, args, bits);
      serialize_sign(sign, bits);
    }
    Statement::Run { expr, sign } => {
      serialize_fixlen(4, &u256(2), bits);
      serialize_term(expr, bits);
      serialize_sign(sign, bits);
    }
    Statement::Reg { name, ownr, sign } => {
      serialize_fixlen(4, &u256(3), bits);
      serialize_name(name, bits);
      serialize_fixlen(128, &u256(*ownr), bits);
      serialize_sign(sign, bits);
    }
  }
}

pub fn deserialize_statement(bits: &BitVec, index: &mut u128) -> Option<Statement> {
  let tag = deserialize_fixlen(4, bits, index)?.low_u128();
  match tag {
    0 => {
      let name = deserialize_name(bits, index)?;
      let args = deserialize_list(deserialize_name, bits, index)?;
      let func = deserialize_func(bits, index)?;
      let init = deserialize_term(bits, index)?;
      let sign = deserialize_sign(bits, index)?;
      Some(Statement::Fun { name, args, func, init, sign })
    }
    1 => {
      let name = deserialize_name(bits, index)?;
      let args = deserialize_list(deserialize_name, bits, index)?;
      let sign = deserialize_sign(bits, index)?;
      Some(Statement::Ctr { name, args, sign })
    }
    2 => {
      let expr = deserialize_term(bits, index)?;
      let sign = deserialize_sign(bits, index)?;
      Some(Statement::Run { expr, sign })
    }
    3 => {
      let name = deserialize_name(bits, index)?;
      let ownr = deserialize_fixlen(128, bits, index)?.low_u128();
      let sign = deserialize_sign(bits, index)?;
      Some(Statement::Reg { name, ownr, sign })
    }
    _ => None,
  }
}

pub fn serialized_statement(statement: &Statement) -> BitVec {
  let mut bits = BitVec::new();
  serialize_statement(statement, &mut bits);
  return bits;
}

pub fn deserialized_statement(bits: &BitVec) -> Option<Statement> {
  deserialize_statement(bits, &mut 0)
}

// Many statements

pub fn serialize_statements(statements: &[Statement], bits: &mut BitVec) {
  serialize_list(serialize_statement, statements, bits);
}

pub fn deserialize_statements(bits: &BitVec, index: &mut u128) -> Option<Vec<Statement>> {
  deserialize_list(deserialize_statement, bits, index)
}

pub fn serialized_statements(statements: &[Statement]) -> BitVec {
  let mut bits = BitVec::new();
  serialize_statements(statements, &mut bits);
  return bits;
}

pub fn deserialized_statements(bits: &BitVec) -> Option<Vec<Statement>> {
  deserialize_statements(bits, &mut 0)
}
