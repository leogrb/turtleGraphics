// BIF5 FUS - Final Project - PartOne
// by Leo Gruber (if18b113)
#if INTERACTIVE
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"
#else
namespace Turtle
#endif

[<AutoOpen>]
module PartOne =
    open System
    type Value = float

    type Cmd =
        | Forward of Value
        | Left    of Value
        | Right   of Value  

    type Program = list<Cmd>

    type Vec2 = float * float
    type Angle = float

    type TurtleState = 
        {
            direction : Angle     // direction in degrees
            position : Vec2       // current position
            trail    : list<Vec2> // points produced so far
        }

 // compute new position out of state and moving value
 // assumption: 0/360 degrees -> North, 90 degrees -> East, 180 degrees -> South, 270 degrees -> West
    let computeForwardMove (s : TurtleState) (f : Value) =
        let d = s.direction
        let p = s.position
        let x = fst p
        let y = snd p
        let rad = d * (Math.PI / float 180)
        let pNew = (x + ((sin rad) * f), y + ((cos rad) * f))
        let trailNew = s.trail @ [pNew]
        {s with position = pNew; trail = trailNew}

    let computeLeftMove (s : TurtleState) (f : Value) =
        let dNew = (s.direction + float 270) % float 360
        computeForwardMove {s with direction = dNew} f

    let computeRightMove (s : TurtleState) (f : Value) =
        let dNew = (s.direction + float 90) % float 360
        computeForwardMove {s with direction = dNew} f

    let interpretCommand (s : TurtleState) (cmd : Cmd) =
        match cmd with
        | Forward v -> 
            computeForwardMove s v
        | Left v -> 
            computeLeftMove s v
        | Right v -> 
            computeRightMove s v

    let interpretTurtleProgram (s : TurtleState) (commands : Program) =
        List.fold interpretCommand s commands

    module Examples =

        let quad = 
            let program =
                [ Forward 30.0; Left 90.0; Forward 30.0; Left 90.0; 
                  Forward 30.0; Left 90.0; Forward 30.0 ]
            program, (50.0,50.0)

        let quad2 = 
            let program =
                [ Forward 10.0; Left 10.0; Right 10.0; Left 10.0;
                  Right 10.0; Left 10.0; Left 10.0;Left 10.0;
                  Right 10.0; Left 10.0; Right 10.0; Left 10.0;]
            program, (50.0,50.0)

        let square = 
            let program =
                [ Forward 10.0; Left 10.0; Left 10.0; Left 10.0; 
                   ]
            program, (50.0,50.0)

        let spiral =
            let program =
                [
                    for lineLen in [20.0 .. (-2.00) .. 0.0] do
                        yield Forward lineLen
                        yield Left 2.0
                ]
            program, (50.0,50.0)


    let runTurtleProgram startPos (p : Program) : list<Vec2> =
        let initialState = { direction = 0.0; position = startPos; trail = [startPos] }
        let resultState = interpretTurtleProgram initialState p
        resultState.trail |> List.rev

module Parser =

        open FParsec

        let pOpenParens = pchar '('
        
        let pCloseParens = pchar ')'

        let pFloatBetweenParens = between pOpenParens pCloseParens pfloat

        let pForward : Parser<Cmd,unit> = 
            skipString "Forward" >>. pFloatBetweenParens >>= (fun a -> preturn (Forward a))
      
        let pRight : Parser<Cmd,unit> = 
            skipString "Right" >>. pFloatBetweenParens >>= (fun a -> preturn (Right a))

        let pLeft : Parser<Cmd,unit> = 
            skipString "Left" >>. pFloatBetweenParens >>= (fun a -> preturn (Left a))

        let pCmd : Parser<Cmd,unit> =
            choice [
                pForward
                pLeft
                pRight
            ]

        let pProgram : Parser<Program,unit> =
            manyTill (pCmd .>> skipChar ';' .>> spaces) eof

        let parseProgram (s : string) : ParserResult<Program,unit> =
            run pProgram s 

        module Examples = 

            let programParserResult (p : ParserResult<Program,unit>) =
                match p with
                | Success(result, _, _) -> result
                | Failure(errorMsg, _, _) -> []

            let testStairs = """Forward(10.0); Left(10.0); Right(10.0); Left(10.0);
                Right(10.0); Left(10.0); Right(10.0);"""

            let test1 = """Forward(30.0); Left(90.0); Forward(30.0); Left(90.0); 
                Forward(30.0); Left(90.0); Forward(30.0);"""    