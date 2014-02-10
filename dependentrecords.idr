module dependentrecords

-- records can only have one constructor...

record Person : Type where
  MkPerson : (name : String) -> (age : Int) -> Person

rodrigo : Person
rodrigo = MkPerson "Rodrigo" 34

-- record update

joao : Person
joao = record {name = "Joao"} rodrigo
