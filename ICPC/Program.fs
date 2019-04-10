module ICPC
open System

let commaSprinkler (input:string) : string option = 
 match (String.length input) < 2 with
 |true -> None //invalid string
 |false -> match (input.[0]=' ',input.[0]=',',not (input.[(String.length input)-1]='.')) with 
           |true,_,_ |_,true,_ |_,_,true -> None //invalid string
           |_ -> 
   let inList = List.ofSeq input in  

   let rec valid (lst:char list) = 
    match lst.[0]=lst.[(List.length lst)-1] with 
    |true -> "valid"
    |false -> match (not (lst.[0]=' '),not (lst.[0]=','),not (lst.[0]='.'),not (Char.IsLower lst.[0])) with
              |true,_,_,_ |_,true,_,_ |_,_,true,_ |_,_,_,true -> "not valid"
              |_ -> valid lst.[1..]
    
    match valid inList with 
    |"not valid" -> None
    |_ -> 
    let rec findcomma (lst:char list) (idx:int) : int option = //returns the index(int option) of the first occurence of a comma(after a given index) else returns None
     match lst.[idx]=lst.[(List.length (lst))-1] with
     |true -> None       //if there are no commas
     |false -> match lst.[idx] with 
               |',' -> Some idx
               |_ -> findcomma inList (idx+1)
  
    match findcomma inList 0 with 
    |None -> Some input
    |Some a -> let idx = a 
              
               match inList.[idx-1]=' ',not (inList.[idx+1]=' ') with
               |true,_ |_,true -> None
               |_ -> 
 
               let rec wordbefore (lst:char List) (idx:int) (lst2:char List) : char List option= //returns the word(as a char list) before the the seplyed index
                match (lst.[idx] = lst.[0]) with 
                |true -> Some lst2         //at the begining of the sentance
                |false -> match lst.[idx-1] with 
                          |' ' -> Some lst2
                          |a -> wordbefore lst (idx-1) (a::lst2)
  
               let Bword = 
                match wordbefore inList idx [] with 
                |Some a -> a
                |None -> []

               let rec wordafter (lst:char list) (idx:int) (lst2:char list) : char list option= ////returns the word(as a char list) after the the seplyed index (index of comma +1)
                match (lst.[idx]=lst.[(List.length (lst))-1]) with
                |true -> Some lst2     //at the end of the sentance 
                |false -> match lst.[idx+1] with 
                          |' ' -> Some lst2
                          |'.' -> Some lst2
                          |a -> wordafter lst (idx+1) (lst2@[a])
 
               let Aword = 
                match wordafter inList (idx) [] with
                |Some a -> a
                |None -> []
               
               let rec matchwordbefore (lst:char list) (word:char list) (idx:int) (widx:int) (count:int): int option= //returns the starting index(int option) of the first occurance of a matching word after a serplyed index
                match (lst.[idx]=word.[(List.length word)-1]) with
                |true -> Some (idx-count)      //at the end of the word
                |false -> match (lst.[idx] = word.[widx]) with
                          |true -> matchwordbefore lst word (idx+1) (widx+1) (count+1)
                          |false -> matchwordbefore lst word (idx+1) widx count 
  
 
               let idxwordbefore = matchwordbefore inList Bword 0 0 0


               Some "pie"

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
