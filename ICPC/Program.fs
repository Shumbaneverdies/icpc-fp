module ICPC
open System

let commaSprinkler (input:string) : string option = 
 let valid1 (str:string) : string option = //checks the input for validiy
  match (str|>String.length) < 2 with
  |true -> None //input too short
  |false -> match (str.[0]=' ',str.[0]=',',not (str.[(str|>String.length)-1]='.')) with 
           |true,_,_ |_,true,_ |_,_,true -> None //invalid string
           |_ -> Some str

 let rec valid2 (str:string option) (idx:int): string option = //checks that all chars are valid
  match str with
  |None -> None
  |Some a -> match a.[idx]=a.[(a|>String.length)-1] with 
             |true -> str
             |false -> match (a.[idx]=' ',a.[idx]=',',a.[idx]='.',Char.IsLower a.[idx]) with
                       |true,_,_,_ |_,true,_,_ |_,_,true,_ |_,_,_,true -> valid2 str (idx+1)
                       |_ -> None

 let rec findcomma (lst:char list) (idx:int) : int option = //returns the index(int option) of the first occurence of a comma(after a given index) else returns None
  match lst.[idx]=lst.[(List.length (lst))-1] with
  |true -> None       //if there are no commas
  |false -> match lst.[idx] with 
            |',' -> Some idx
            |_ -> findcomma lst (idx+1)

 let rec findperiod (lst:char list) (idx:int) : int option = //returns the index(int option) of the first occurence of a period(after a given index) else returns None
  match lst.[idx] with 
  |'.' -> Some idx
  |_ -> findperiod lst (idx+1)
 
 let rec valid3 (str:string option) idx : string option = //checks that there is a space after each comma, no space before a comma and no space too positions after a comma
  match str with 
  |None -> None
  |Some a -> let list = a|>List.ofSeq
             match (findcomma list idx) with
             |None -> str                                                         //no comma thus i am done! 
             |Some CIDX -> match a.[CIDX-1]=' ',a.[CIDX+1]=' ',a.[CIDX+2]=' ' with    //CIDX is the index of the comma
                           |true,_,_ |_,false,_ |_,_,true -> None
                           |_ -> valid3 str (CIDX+1)

 let rec valid4 (str:string option) (idx:int) : string option = //simular to valid3 but with periods
  match str with 
  |None -> None
  |Some a -> let list = a|>List.ofSeq
             match (findperiod list idx) with 
             |None -> None //should never reach this point
             |Some PIDX -> match PIDX = ((List.length (list))-1) with                                      //PIDX is the index of the period
                           |true -> str 
                           |false -> match a.[PIDX-1]=' ',a.[PIDX+1]='.',a.[PIDX+2]='.',a.[PIDX+2]=' ' with
                                     |true,_,_,_ |_,false,_,_ |_,_,true,_ |_,_,_,true -> None
                                     |_ -> valid4 str (PIDX+1)



 let input = valid1 input
 let input = valid2 input 0
 let input = valid3 input 0
 let input = valid4 input 0 
 
 input
let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
