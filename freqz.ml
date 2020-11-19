open Owl;;
open Owl_plplot;;
let pi = Owl_const.pi;;   (*pi*)


let resize r x =                    (*zero pad the array to 2*x length*)
let z = Array.make (2*x-(Array.length r)) 0. in
let y=Array.append r z in
Arr.of_array y [|2*x|] |> Dense.Ndarray.Generic.cast_d2z
;;



let dtft r x=                  (*dtft for upper unit circle (i.e whole if false)*)
let a = resize r x in
let b = Owl_fft.D.fft a in
Dense.Ndarray.Z.get_slice [[0;x-1]] b;;



let dtftw r x=              (*dtft for full circle (i.e whole is true)*)
let a = resize r x in
Owl_fft.D.fft a;;


let freq n =         (*n is the number of frequencies where freqz is to be calculated*)
let w = Arr.linspace 0. pi (n+1) in
Arr.get_slice [[0;n-1]] w;;

let freqf n =         (*n is the number of frequencies where freqz is to be calculated (if whole is true)*)
let w = Arr.linspace 0. (2. *. pi) (2*n+1) in
Arr.get_slice [[0;(2*n-1)]] w;;


let freqz ?(n=512) ?(whole=false) b a = (*b represents numerator array while a represent denominator array*)

if whole=true then
let x = dtftw a n in
let y = dtftw b n in
Dense.Ndarray.Z.div y x else	

let x = dtft a n in
let y = dtft b n in
Dense.Ndarray.Z.div y x;;



let mag ?(m=512) ?(full=false) b a =  (*magnitude of freqz*)
let z = freqz ~n:m ~whole:full b a in
let x = Dense.Ndarray.Z.abs z in
Dense.Ndarray.Z.re x
;;


let a=[|1.;0.;(1. /. 3.);0.|];;                (* test case*)
let b= [|(1. /. 6.);0.5;0.5;(1. /. 6.)|];;     (*test case*)

let z = mag ~full:true b a;;       (*magnitude Ndarray*)
let n=float (Arr.numel z) -. 1.0 ;; (*length of z - 1*)
let s = pi /. 512.;;

let g x =Arr.get z [|int_of_float (x /. s)|] in   (*z is magitude array of the freqz (mag function) youre trying to plot *)
let h = Plot.create "butterworth4.png" in
Plot.set_title h "test";
Plot.set_xlabel h "x-axis";
Plot.set_ylabel h "y-axis";
Plot.set_font_size h 8.;
Plot.set_pen_size h 3.;
Plot.plot_fun ~h g (0. *. s) (n *. s);       (*if no of frequencies are m then instead 511 use m-1*)
Plot.output h;;











