module ICPC
open System

let commaSprinkler (input:string) : string option = 
 match (String.length input) = 0 with
 |true -> None //empty string
 |false -> 
 let inList = List.ofSeq input in  
  let rec findcomma (lst:char list) (idx:int) : int option = //returns the index(int option) of the first occurence of a comma(after a given index) else returns None
   match lst.[idx]=lst.[(List.length (lst))-1] with
   |true -> None       //if there are no commas
   |false -> match lst.[idx] with 
             |',' -> Some idx
             |_ -> findcomma inList (idx+1)
  
  let idx = 
   match findcomma inList 0 with 
   |Some a -> a
   |None -> -1

 let rec wordbefore (lst:char List) (idx:int) (lst2:char List) : char List option= //returns the word(as a char list) before the the seplyed index
  match idx with 
  | -1 -> None
  | _ -> match (lst.[idx] = lst.[0]) with 
         |true -> Some lst2         //at the begining of the sentance
         |false -> match lst.[idx-1] with 
                   |' ' -> Some lst2
                   |a -> wordbefore lst (idx-1) (a::lst2)
 
 let Bword = 
  match wordbefore inList idx [] with 
  |Some a -> a
  |None -> []

 let rec wordafter (lst:char list) (idx:int) (lst2:char list) : char list option= ////returns the word(as a char list) after the the seplyed index
  match idx with 
  | -1 -> None
  | _ -> match (lst.[idx]=lst.[(List.length (lst))-1]) with
         |true -> Some lst2     //at the end of the sentance
         |false -> match lst.[idx+1] with 
                   |' ' -> wordafter lst (idx+1) lst2
                   |'.' -> Some lst2
                   |a -> wordafter lst (idx+1) (lst2@[a])
 
 let Aword = 
  match wordafter inList idx [] with
  |Some a -> a
  |None -> []

 None
let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
