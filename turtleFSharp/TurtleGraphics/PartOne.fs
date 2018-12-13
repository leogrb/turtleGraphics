#if INTERACTIVE
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/FParsec/lib/net40-client/FParsec.dll"
#else
namespace Turtle.WS17
#endif


[<AutoOpen>]
module PartOne =
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

    let interpretTurtleProgram (s : TurtleState) (commands : Program) =
        failwith "TODO"


    module Examples =
    
        let quad = 
            let program =
                [ Forward 30.0; Left 90.0; Forward 30.0; Left 90.0; 
                  Forward 30.0; Left 90.0; Forward 30.0 ]
            program, (50.0,50.0)

        let spiral =
            let program =
                [
                    for lineLen in [100.0 .. (-2.00) .. 0.0] do
                        yield Forward lineLen
                        yield Left 90.0
                ]
            program, (0.0,0.0)

    let runTurtleProgram startPos (p : Program) : list<Vec2> =
        let initialState = { direction = 90.0; position = startPos; trail = [startPos] }
        let resultState = interpretTurtleProgram initialState p
        resultState.trail |> List.rev


    module Parser =

        open FParsec

        let pForward : Parser<Cmd,unit> = 
            failwith "TODO"
        
        let pRight : Parser<Cmd,unit> = 
            failwith "TODO"

        let pLeft : Parser<Cmd,unit> = 
            failwith "TODO"

        let pCmd : Parser<Cmd,unit> =
            failwith "TODO"

        let pProgram : Parser<Program,unit> =
            failwith "TODO"

        let parseProgram (s : string) : ParserResult<Program,unit> =
            run pProgram s 


        module Examples = 
        
            let test1 = """Forward(30.0); Left(90.0); Forward(30.0); Left(90.0); 
    Forward(30.0); Left(90.0); Forward(30.0);"""
