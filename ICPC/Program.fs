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
  match idx=(List.length (lst))-1 with
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
                           |true -> match a.[PIDX-1]=' ',a.[PIDX-2]='.' with
                                    |true,_ |_,true -> None
                                    |_ -> str 
                           |false -> match a.[PIDX-1]=' ',a.[PIDX+1]='.',a.[PIDX+2]='.',a.[PIDX+2]=' ' with
                                     |true,_,_,_ |_,true,_,_ |_,_,true,_ |_,_,_,true -> None
                                     |_ -> valid4 str (PIDX+1)
 
 let validation (str:string) : string option = 
  let output = valid1 str
  let output = valid2 output 0
  let output = valid3 output 0
  let output = valid4 output 0
  output 

 let rec wordbefore (lst:char List) (idx:int) (lst2:char List) : char list option = //returns the word before the serplyed index (as char list) 
  match (lst.[idx] = lst.[0]) with 
  |true -> Some (lst.[0]::lst2)         //at the begining of the sentance
  |false -> match lst.[idx-1] with 
            |' ' -> Some lst2
            |a -> wordbefore lst (idx-1) (a::lst2)

 let rec wordafter (lst:char list) (idx:int) (lst2:char list) : char list option= //returns the word after the serplyed index (as char list) 
  match (lst.[idx]=lst.[(List.length (lst))-1]) with
  |true -> Some lst2     //at the end of the sentance 
  |false -> match lst.[idx+1] with 
            |' ' -> Some lst2
            |'.' -> Some lst2
            |a -> wordafter lst (idx+1) (lst2@[a])

 let rec matchwordbefore (str:char list) (word:char list) (sidx:int) (widx:int) : int option = //finds the word in the sentance and gives back the index of the 'spot' after the word
  match str.[sidx]=str.[(List.length str)-1] with
  |true -> None //at the end of the sentance
  |false -> match word.[widx]=word.[(List.length word)-1] with
            |true -> Some (sidx+1) //at the end of the word (ie match found)
            |false -> match str.[sidx]=word.[widx] with
                      |true -> matchwordbefore str word (sidx+1) (widx+1)
                      |false -> matchwordbefore str word (sidx+1) widx

 let addcomma (str:char list) (idx:int) : char list = //puts a comma in at a serplyed index
  let Fstr = str.[0..(idx-1)]
  let Sstr = str.[(idx+1)..]
  let Sstr = ','::Sstr
  let out = Fstr@Sstr
  out

 let rec makestr (chl:char list) str : string = //converts a char list into a string
  match chl with
  |h::t -> match List.length t = 0 with 
           |true -> str
           |false -> makestr t (str + Char.ToString (h))

 let rec sprinkleAfter (str:string option) idx : string option = 
  match str with 
  |None -> None
  |Some a -> 
   let strL = List.ofSeq a //convert it to a char list
   let idx = match findcomma strL idx with //converting to an int //index of the first comma
             |Some a -> a
             |_ -> -1  

   match idx = -1 with
   |true -> str
   |false -> 

   let Bword = match (wordbefore strL idx []) with 
               |Some a -> a
               |_ -> [] //should never reach here
   
   let inIDX = match matchwordbefore strL Bword idx idx with //inIDX is the place a comma must be put in
               |Some a -> a
               |None -> -1
   
   match inIDX with 
   | -1 -> Some (makestr strL "") 
   |_ -> 
    let strL = addcomma strL inIDX
    let str2 = makestr strL ""

    let strO = match str2 with 
               |a -> Some a
               
    match strO=str with
    |true -> strO
    |_ -> sprinkleAfter strO inIDX
    


 let output = input|>validation 

 let output = sprinkleAfter output 0

 output

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
