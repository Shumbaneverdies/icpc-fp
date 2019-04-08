module ICPC
open System

let commaSprinkler input = 
 let inList = List.ofSeq input in  
  let rec findcomma (lst:string List) (idx:int) : int option = //returns the index(int option) of the first occurence of a comma else returns None
   match lst.[0]=lst.[(List.length (lst))-1] with
   |true -> None       //if there are no commas
   |false -> match lst.[0] with 
             |"," -> Some idx
             |_ -> findcomma (inList.[1..]) (idx+1)
  let idx = 
   match findcomma inList 0 with 
   |Some a -> a
   |None -> -1

 let rec wordbefore (lst:string List) (idx:int) lst2 : string List = //returns the word(as a list) before the the seplyed index
  match (lst.[idx] = lst.[0]) with 
  |true -> lst2         //at the begining of the sentance
  |false -> match lst.[idx-1] with 
            |" " -> lst2
            |a -> wordbefore lst (idx-1) (a::lst2)
 wordbefore input idx [] 



let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
