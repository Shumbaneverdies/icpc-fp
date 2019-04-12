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
  match idx = 0 with        //building the word backwards
  |true -> Some lst2         //at the begining of the sentance
  |false -> match lst.[idx-1] with 
            |' ' -> Some lst2
            |a -> wordbefore lst (idx-1) (a::lst2)

 let rec wordafter (lst:char list) (idx:int) (lst2:char list) (first:bool) : char list option= //returns the word after the serplyed index (as char list) 
  match idx=List.length (lst)-1 with
  |true -> Some lst2    //at the end of the sentance 
  |false -> match first with
            |true -> wordafter lst (idx+1) lst2 (not first) //every comma must have a space after it
            |false -> match lst.[idx+1] with 
                      |' ' |'.' |',' -> Some lst2
                      |a -> wordafter lst (idx+1) (lst2@[a]) first

 let rec matchword (str:char list) (word:char list) (sidx:int) (widx:int) : int option = //finds the word in the sentance and gives back the index of the 'spot' after the word
  match sidx=(List.length str)-1 with                                              //idx = where it starts lokking from for the matching word (Bword) in the string
  |true -> None //at the end of the sentance
  |false -> match widx=(List.length word)-1 with
            |true -> Some (sidx+1) //at the end of the word (ie match found)
            |false -> match str.[sidx]=word.[widx] with
                      |true -> matchword str word (sidx+1) (widx+1)
                      |false -> matchword str word (sidx+1) 0

 let addcomma (str:char list) (idx:int) : char list = //puts a comma in at a serplyed index and a space after it
  let Fstr = str.[0..(idx-1)]
  let Sstr = str.[(idx)..]
  let Sstr = ','::Sstr
  let out = Fstr@Sstr
  out

 let rec makestr (chl:char list) str : string = //converts a char list into a string
  match chl with
  |h::t -> match List.length t = 0 with 
           |true -> (str + Char.ToString (h))
           |false -> makestr t (str + Char.ToString (h))
  |_ -> failwith "someting went wrong" //should never reach here
 
 let mkstroption (str:char list) : string option = Some (makestr str "")

 let rec sprinkleAfter (str:string option) idx : string option = //places a comma after each word that deserves it starting at the given index
  match str with 
  |None -> None
  |Some a -> 
  let strL = List.ofSeq a //convert it to a char list
  let CIDX = match findcomma strL idx with //converting to an int //index of the first comma
             |Some a -> a
             |_ -> -1  

  match CIDX = -1 with
  |true -> str
  |false -> 
  let Bword = match (wordbefore strL CIDX []) with 
              |Some a -> a
              |_ -> [] 
   
  let inIDX = match matchword strL Bword idx 0 with //inIDX is the place a comma must be put in             idx = where it starts lokking from for the matching word (Bword) in the string
              |Some a -> a
              |None -> -1 
   
  match Bword with                                                                        
  |[] -> str 
  |_ -> match inIDX with
        | -1 -> str
        | _ -> match strL.[inIDX] with
               |',' |'.' -> sprinkleAfter str (idx+2)
               |' ' -> sprinkleAfter (mkstroption (addcomma strL inIDX)) (idx+2)

 let rec sprinkleBefore (str:string option) idx : string option = //simular to above
  match str with 
  |None -> None
  |Some a -> 
  let strL = List.ofSeq a //convert it to a char list
  let CIDX = match findcomma strL idx with //converting to an int //index of the first comma
             |Some a -> a
             |_ -> -1  

  match CIDX = -1 with
  |true -> str
  |false -> 
  let Aword = match (wordafter strL CIDX [] true) with 
              |Some a -> a
              |_ -> [] 
   
  let inIDX = match matchword strL Aword idx 0 with //inIDX is the place a comma must be put in             idx = where it starts lokking from for the matching word (Bword) in the string
              |Some a -> a
              |None -> -1 
   
  match Aword with                                                                        
  |[] -> str 
  |_ -> match inIDX with
        | -1 -> str
        | _ -> match strL.[inIDX] with
               |',' |'.' -> sprinkleBefore str (idx+2)
               |' ' -> sprinkleBefore (mkstroption (addcomma strL inIDX)) (idx+2)
 
 let rec sprinkle (str:string option) : string option =
  let ori = str
  let str = sprinkleAfter str 0
  let str = sprinkleBefore str 0
  match ori=str with
  |true -> str
  |false -> sprinkle str 

//^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^FUNCTIONS^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

 let output = input|>validation 

 //let output = sprinkle output
 let output = sprinkleAfter output 0
 let output = sprinkleBefore output 0
 output

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
