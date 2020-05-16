open Base
open Dict

type phone = { taxon: string; s: string }

type phone_pair = phone * phone

type word_pair_data =
  | Cognate of Probability.t * phone_pair list
  | Not_cognate of Probability.t * phone list

let maximise data =
    let 
