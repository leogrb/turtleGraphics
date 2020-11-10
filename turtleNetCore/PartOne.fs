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
    let computeForwardMove (s : TurtleState) (f : Value) =
        let d = s.direction
        let p = s.position
        let x = fst p
        let y = snd p
        let rad = d * (Math.PI / float 180)
        let pNew = (x + ((cos rad) * f), y + ((sin rad) * f))
        let trailNew = s.trail @ [pNew]
        {s with position = pNew; trail = trailNew}

    let computeLeftMove (s : TurtleState) (f : Value) =
        let dNew = (s.direction + float 90) % float 360
        computeForwardMove {s with direction = dNew} f

    let computeRightMove (s : TurtleState) (f : Value) =
        let dNew = (s.direction - float 90) % float 360
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
