open Base

(** Is a consonant *)
let cons t =
  not
    (List.exists
       ~f:(fun c -> String.is_prefix ~prefix:c t)
       [
         "ɹ";
         "ɻ";
         "j";
         "w";
         "ʔ";
         "i";
         "y";
         "ɨ";
         "u";
         "e";
         "ø";
         "ʌ";
         "o";
         "æ";
         "ɶ";
         "a";
         "ɑ";
         "ɒ";
         "ɪ";
         "ʏ";
         "ɯ";
         "ʊ";
         "ɛ";
         "œ";
         "ə";
         "ɔ";
       ])

(** Is syllabic *)
let syl t =
  List.exists
    ~f:(fun c -> String.is_prefix ~prefix:c t)
    [
      "i";
      "y";
      "ɨ";
      "u";
      "e";
      "ø";
      "ʌ";
      "o";
      "æ";
      "ɶ";
      "a";
      "ɑ";
      "ɒ";
      "ɪ";
      "ʏ";
      "ɯ";
      "ʊ";
      "ɛ";
      "œ";
      "ə";
      "ɔ";
    ]
