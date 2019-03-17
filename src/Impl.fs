
(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Impl.fsi: implementazioni degli studenti
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Impl

open Absyn
open Prelude
open System

// converte float in rational
let rationalize (x : float) : rational =
    let numerator =
        string x                                                    // trasforma x in stringa
        |> String.filter (fun c -> c <> '.')                        // rimuove il '.'
        |> Int32.Parse                                              // trasforma la stringa in Int32
    in rational (numerator, if x = 0. then 1 else int (round (float numerator / x)))      // restituisce il rational


// restituisce il grado di un monomio
let monomial_degree (Monomial (_, exp) : monomial) : int = exp


// restituisce un monomial negato
let monomial_negate (Monomial (q, exp) : monomial) : monomial = Monomial (-q, exp)


// restituisce il grado di un polynomial
let polynomial_degree (Polynomial ms : polynomial) : int =
    let (Monomial (_, exp)) = List.maxBy monomial_degree ms     // restituisce il monomial di grado maggiore 
    in exp


// restituisce un polynomial negato
let polynomial_negate (Polynomial ms : polynomial) : polynomial =
    Polynomial (List.map monomial_negate ms)                    // applica monomial_negate ad ogni monomial del polynomial


// restituisce il grado di un normalized_polynomial
let normalized_polynomial_degree (NormalizedPolynomial q_array : normalized_polynomial) : int =
    if q_array = [||] then 0            // il grado è 0 se l'array è vuoto
    else Array.length q_array - 1       // altrimenti la sua lunghezza - 1


// normalizza un polynomial restituendo il corrispondente normalized_polynomial
let normalize (Polynomial ms as p : polynomial) : normalized_polynomial =
    // rimuove gli zeri finali da una lista di rational
    let rec remove_trailing_zeros ls = 
        match ls with
        | [] -> []
        | x :: xs ->
            let cleaned_list = remove_trailing_zeros xs     // rimuove gli zeri finali dalla coda
            match cleaned_list with
            | [] -> if x = 0Q then [] else [x]              // se la nuova coda è vuota e l'elemento corrente non è uno zero allora lo aggiunge alla coda
            | _ -> x :: cleaned_list                        // se la nuova coda non è vuota allora agginge l'elemento corrente in testa

    in NormalizedPolynomial (                   // crea il normalized_polynomial da restituire
        List.init                               // crea un lista chiamando il generatore n volte
        <| (polynomial_degree p + 1)            // dimensione_lista = degree + 1, 
        <| (fun i -> List.sumBy (fun (Monomial (coeff, exp)) -> if exp = i then coeff else 0Q) ms)      // genera un elemento (rational) della lista sommando il coefficiente dei monomi di grado uguale a index (i)
        |> remove_trailing_zeros                // rimuove gli zeri finali dalla lista di rational ottenuta
        |> List.toArray                         // converte la lista in un array
    )


// deriva un polynomial
let derive (Polynomial ms : polynomial) : polynomial =
    // deriva un monomial
    let derive_monomial (Monomial (q, exp)) = Monomial (rational exp * q, if exp = 0 then 0 else exp - 1)
    in Polynomial (List.map derive_monomial ms)     // applica derive_monomial ad ogni monomial del polynomial


// semplifica una expr, restituendo il polynomial da essa rappresentato
let rec reduce (e : expr) : polynomial = 
    match e with
    | Poly p -> p                           // se è un polynomial lo restituisce
    | Derive e -> derive (reduce e)         // se è una derivata ricava il polynomial e lo deriva


// risolve l'equazione di grado zero ottenuta eguagliando il normalized_polynomial in input a 0
let solve0 (NormalizedPolynomial q_array : normalized_polynomial) : bool =
    q_array = [||]          // q_array vuoto significa 0 = 0 quindi true


// risolve l'equazione di primo grado ottenuta eguagliando il normalized_polynomial in input a 0
let solve1 (NormalizedPolynomial q_array : normalized_polynomial) : rational =
    -q_array.[0] / q_array.[1]          // x = -b / a


// risolve l'equazione di secondo grado ottenuta eguagliando il normalized_polynomial in input a 0
let solve2 (NormalizedPolynomial q_array : normalized_polynomial) : (float * float option) option =
    let a = q_array.[2]
    let b = q_array.[1]
    let c = q_array.[0]
    let delta = (rational.Pow (b, 2) - 4Q * a * c)      // delta = b^2 - 4ac

    if delta < 0Q then      // nessuna soluzione
        None

    elif delta = 0Q then    // una soluzione
        Some (float (-b / (2Q * a)), None)

    else                    // due soluzioni
        Some ((float(-b) + rational.Sqrt delta) / (2. * float a), Some ((float(-b) - rational.Sqrt delta) / (2. * float a)))


// risolve l'equazione di terzo grado ottenuta eguagliando il normalized_polynomial in input a 0
let solve3 (NormalizedPolynomial q_array : normalized_polynomial) : (float * float option * float option) =
    // determina l'unica radice reale (una soluzione reale)
    let solve_1_root (a, d) =
        let x = -Math.Pow (float (d / a), 1. / 3.)
        in (x, None, None)

    // determina le tre radici reali (tre soluzioni reali)
    let solve_3_roots (a, b, g, delta) =
        let rad = sqrt ((g ** 2.) / 4. - delta)
        let angle = Math.Acos (float (-g) / (2. * rad))
        let n = (Math.Sqrt 3.) * (Math.Sin (angle / 3.))
        
        let x1 = 2. * (Math.Pow (rad, 1. / 3.)) * (Math.Cos (angle / 3.)) + (-b / (3. * float a))
        let x2 = (-Math.Pow (rad, 1. / 3.)) * ((Math.Cos (angle / 3.)) + n) + (-b / (3. * float a));
        let x3 = (-Math.Pow (rad, 1. / 3.)) * ((Math.Cos (angle / 3.)) - n) + (-b / (3. * float a));

        if x2 = x3 || x1 = x3 then              // 3 soluzioni (2 coincidenti)
            (x1, Some x2, None)
        else                                    // 3 soluzioni distinte
            (x1, Some x2, Some x3)

    // determina l'unica radice reale (una soluzione reale e due soluzioni complesse)
    let solve_real_compl_roots (a, b, g, delta) =
        let r1 = - float (g / 2.) + sqrt delta
        let s1 = if r1 >= 0. then Math.Pow (float r1, 1. / 3.) else -Math.Pow (-r1, 1. / 3.)
        let r2 = -(g / 2.) - sqrt delta
        let s2 = if r2 >= 0. then Math.Pow (r2, 1. / 3.) else -Math.Pow (-r2, 1. / 3.)

        let x = s1 + s2 - (b / (3. * a))
        in (x, None, None)

    // coefficienti
    let a = float q_array.[3]
    let b = float q_array.[2]
    let c = float q_array.[1]
    let d = float q_array.[0]

    let f = ((3. * c / a) - (b ** 2.) / (a ** 2.)) / 3.
    let g = (((2. * (b ** 3.)) / (a ** 3.)) + ((-9. * b * c) / (a ** 2.)) + (27. * d / a)) / 27.
    let delta = ((g ** 2.) / 4. + (f ** 3.) / 27.)

    if delta = 0. && f = 0. && g = 0. then      // una soluzione reale
        solve_1_root (a, d)

    elif delta <= 0. then                       // tre soluzioni reali
        solve_3_roots (a, b, g, delta)

    else                                        // una soluzione reale e due soluzioni complesse
        solve_real_compl_roots (a, b, g, delta)
