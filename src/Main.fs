(*
 * SharpSolver - Progetto di Programmazione e Calcolo a.a. 2018-19
 * Main.fs: console e codice main
 * (C) 2018 Alvise Spano' @ Universita' Ca' Foscari di Venezia
 *)

module SharpSolver.Main

open Microsoft.FSharp.Text.Lexing
open Absyn
open System
open Prelude
open Microsoft.FSharp.Text


// funzioni di logging e printing
//

let hout hd fmt =
    if not <| String.IsNullOrWhiteSpace hd then
        printf "[%s]%s" hd (new String (' ', max 1 (Config.prefix_max_len - String.length hd)))
        stdout.Flush ()
    printfn fmt

let chout col hd fmt =
    let c = Console.ForegroundColor
    Console.ForegroundColor <- col
    Printf.kprintf (fun s -> hout hd "%s" s; Console.ForegroundColor <- c) fmt

let out fmt = hout "" fmt
let cout col fmt = chout col "" fmt

let norm fmt = chout ConsoleColor.Yellow "norm" fmt
let redux fmt = chout ConsoleColor.Magenta "redux" fmt
let sol fmt = chout ConsoleColor.Green "sol" fmt
let ident fmt = chout ConsoleColor.Green "ident" fmt    
let error fmt = chout ConsoleColor.Red "error" fmt   
let degree fmt = hout "degree" fmt

// interprete dei comandi e delle espressioni
//

let interpreter_loop () =
    while true do
        printf "\n%s" Config.prompt_prefix          // stampa il prompt
        stdout.Flush ()                             // per sicurezza flusha lo stdout per vedere la stampa del prompt senza end-of-line
        let input = Console.ReadLine ()             // leggi l'input scritto dall'utente
        let lexbuf = LexBuffer<_>.FromString input  // crea un lexbuffer sulla stringa di input

        // funzione locale per il pretty-printing degli errori
        let localized_error msg =
            let tabs = new string (' ', Config.prompt_prefix.Length + lexbuf.StartPos.Column)
            let cuts = new string ('^', let n = lexbuf.EndPos.Column - lexbuf.StartPos.Column in if n > 0 then n else 1)
            cout ConsoleColor.Yellow "%s%s\n" tabs cuts
            error "error at %d-%d: %s" lexbuf.StartPos.Column lexbuf.EndPos.Column msg 
        
        // blocco con trapping delle eccezioni
        try
            let line = Parser.line Lexer.tokenize lexbuf    // invoca il parser sul lexbuffer usando il lexer come tokenizzatore
            #if DEBUG
            hout "absyn" "%+A" line
            hout "pretty" "%O" line
            #endif

            // interpreta la linea in base al valore di tipo line prodotto dal parsing
            match line with
            | Cmd "help" ->
                out "%s" Config.help_text

            | Cmd ("quit" | "exit") ->
                out "%s" Config.exit_text
                exit 0
            
            | Cmd s -> error "unknown command: %s" s    // i comandi non conosciuti cadono in questo caso


            | Expr e ->
                let poly = Impl.reduce e                            // semplifica l'espressione
                let normalized_poly = Impl.normalize poly           // nomalizza il polynomial

                redux "%O" poly                                     // log [redux]
                norm "%O" normalized_poly                           // log [norm]
                degree "%d" (Impl.normalized_polynomial_degree normalized_poly)      // log [degree] (grado del normalized_polynomial) 


            | Equ (e1, e2) ->
                // funzione per sommare (unire) due polynomial
                let sum_polys (Polynomial ms1) (Polynomial ms2) = Polynomial (ms1 @ ms2)

                // semplifica le due espressioni
                let poly1 = Impl.reduce e1
                let poly2 = Impl.reduce e2

                let normalized_poly =
                    sum_polys (poly1) (Impl.polynomial_negate poly2)        // somma ad un polynomial l'opposto dell'altro
                    |> Impl.normalize                                       // normalizza il polinomial ottenuto

                let norm_poly_degree = Impl.normalized_polynomial_degree normalized_poly    // determina il grado del normalized_polynomial
                
                // log
                redux "%O" (Equ (Poly poly1, Poly poly2))       // log [redux]
                norm "%O = 0" normalized_poly                   // log [norm] (stampa "equazione normalizzata")
                degree "%d" norm_poly_degree                    // log [degree]

                // risolvi l'equazione
                if norm_poly_degree = 0 then        // grado 0
                    ident "%b" (Impl.solve0 normalized_poly)

                elif norm_poly_degree = 1 then      // grado 1
                    sol "x = %O" (Impl.solve1 normalized_poly)

                elif norm_poly_degree = 2 then      // grado 2
                    let solution = Impl.solve2 normalized_poly      // determina le soluzioni
                    match solution with                             // log soluzioni
                    | None -> sol "%s" "Nessuna radice reale"
                    | Some (x, None) -> sol "x = %f" x
                    | Some (x1, Some x2) -> sol "x1 = %f vel x2 = %f" x1 x2
                
                elif norm_poly_degree = 3 then      // grado 3
                    let solution = Impl.solve3 normalized_poly      // determina le soluzioni
                    match solution with                             // log soluzioni
                    | (x1, None, None) -> sol "x1 = %f" x1
                    | (x1, Some x2, None) -> sol "x1 = %f vel x2 = %f" x1 x2
                    | (x1, Some x2, Some x3) -> sol "x1 = %f vel x2 = %f vel x3 = %f" x1 x2 x3
                    | _ -> error "Critical error"

                else                                // grado > 3
                    error "cannot solve equations of degree %d" norm_poly_degree

                   
        // gestione delle eccezioni
        with LexYacc.ParseErrorContextException ctx ->
                let ctx = ctx :?> Parsing.ParseErrorContext<Parser.token>
                localized_error (sprintf "syntax error%s" (match ctx.CurrentToken with Some t -> sprintf " at token <%O>" t | None -> ""))

           | Lexer.LexerError msg -> localized_error msg 

           | :? NotImplementedException as e -> error "%O" e
        
           | e -> localized_error e.Message


// funzione main: il programma comincia da qui
//

[<EntryPoint>]
let main _ = 
    let code =
        try
            interpreter_loop ()                 // chiama l'interprete
            0                                   // ritorna il codice di errore 0 (nessun errore) al sistema operativo se tutto è andato liscio
        with e -> error "fatal error: %O" e; 1  // in caso di eccezione esce con codice di errore 1
    #if DEBUG
    Console.ReadKey () |> ignore                // aspetta la pressione di un tasto prima di chiudere la finestra del terminare 
    #endif
    code


