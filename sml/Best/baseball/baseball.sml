(*
 * Andy's Baseball Game - Standard ML/OpenGL translation.
 *
 * This is a structured SML translation of Best/BASEBALL.BAS.  The original
 * program was a QBASIC two-player game that used SCREEN 7, PSET, DRAW, INKEY$,
 * and COLOR.  This version keeps the same gameplay model while routing all
 * drawing through a small OpenGL surface interface.  Bind the OPENGL_SURFACE
 * signature to the OpenGL/GLUT (or SDL2+OpenGL) package available in your SML
 * environment and instantiate BaseballFn to run it.
 *)

signature OPENGL_SURFACE = sig
  type window
  datatype key = Char of char | Space | Escape | Quit | NoKey

  val openWindow : {title : string, width : int, height : int} -> window
  val clear      : window -> {r : real, g : real, b : real} -> unit
  val color      : window -> {r : real, g : real, b : real} -> unit
  val line       : window -> real * real -> real * real -> unit
  val rect       : window -> real * real -> real * real -> unit
  val circle     : window -> real * real -> real -> unit
  val text       : window -> real * real -> string -> unit
  val present    : window -> unit
  val pollKey    : window -> key
  val delayMs    : int -> unit
end

functor BaseballFn (GL : OPENGL_SURFACE) = struct
  datatype side = Home | Visitors
  datatype lane = LeftLane | MiddleLane | RightLane
  datatype battingResult = Hit | GroundOut | PopOut
  datatype throwTarget = First | Second | Third | HomePlate

  type point = real * real

  type state =
    { inning : int,
      outs : int,
      strikes : int,
      home : int,
      visitors : int,
      runners : bool * bool * bool,
      batting : side,
      message : string }

  val width = 320
  val height = 200
  val frameMs = 16

  fun rgb (r, g, b) = {r = r, g = g, b = b}
  val black = rgb (0.0, 0.0, 0.0)
  val white = rgb (1.0, 1.0, 1.0)
  val green = rgb (0.0, 0.55, 0.0)
  val dirt = rgb (0.55, 0.32, 0.12)
  val blue = rgb (0.10, 0.20, 0.90)
  val red = rgb (0.95, 0.08, 0.08)
  val yellow = rgb (1.0, 0.9, 0.2)
  val grey = rgb (0.6, 0.6, 0.6)

  fun initialState () =
    {inning = 1, outs = 0, strikes = 0, home = 0, visitors = 0,
     runners = (false, false, false), batting = Visitors, message = ""}

  fun battingName Visitors = "VISITORS"
    | battingName Home = "HOME"

  fun battingScore Visitors ({visitors, ...} : state) = visitors
    | battingScore Home ({home, ...} : state) = home

  fun withMessage msg ({inning, outs, strikes, home, visitors, runners, batting, ...} : state) =
    {inning = inning, outs = outs, strikes = strikes, home = home,
     visitors = visitors, runners = runners, batting = batting, message = msg}

  fun addRun Visitors ({inning, outs, strikes, home, visitors, runners, batting, message} : state) =
        {inning = inning, outs = outs, strikes = strikes, home = home,
         visitors = visitors + 1, runners = runners, batting = batting, message = message}
    | addRun Home ({inning, outs, strikes, home, visitors, runners, batting, message} : state) =
        {inning = inning, outs = outs, strikes = strikes, home = home + 1,
         visitors = visitors, runners = runners, batting = batting, message = message}

  fun setRunners runners ({inning, outs, strikes, home, visitors, batting, message, ...} : state) =
    {inning = inning, outs = outs, strikes = strikes, home = home,
     visitors = visitors, runners = runners, batting = batting, message = message}

  fun addOut ({inning, outs, strikes, home, visitors, runners, batting, message} : state) =
    {inning = inning, outs = outs + 1, strikes = 0, home = home,
     visitors = visitors, runners = runners, batting = batting, message = message}

  fun addStrike st =
    let
      val {inning, outs, strikes, home, visitors, runners, batting, ...} = st
      val strikes' = strikes + 1
    in
      if strikes' >= 3 then
        {inning = inning, outs = outs + 1, strikes = 0, home = home,
         visitors = visitors, runners = runners, batting = batting, message = "STRIKE THREE - OUT"}
      else
        {inning = inning, outs = outs, strikes = strikes', home = home,
         visitors = visitors, runners = runners, batting = batting, message = "STRIKE"}
    end

  fun endHalfIfNeeded ({inning, outs, strikes, home, visitors, runners, batting, message} : state) =
    if outs < 3 then {inning = inning, outs = outs, strikes = strikes, home = home,
                      visitors = visitors, runners = runners, batting = batting,
                      message = message}
    else
      case batting of
        Visitors => {inning = inning, outs = 0, strikes = 0, home = home,
                     visitors = visitors, runners = (false, false, false),
                     batting = Home, message = "BOTTOM HALF"}
      | Home => {inning = inning + 1, outs = 0, strikes = 0, home = home,
                 visitors = visitors, runners = (false, false, false),
                 batting = Visitors, message = "NEXT INNING"}

  fun scoreAllRunners st =
    let
      val {runners = (r1, r2, r3), batting, ...} = st
      val st1 = if r1 then addRun batting st else st
      val st2 = if r2 then addRun batting st1 else st1
      val st3 = if r3 then addRun batting st2 else st2
      val st4 = addRun batting st3
    in setRunners (false, false, false) st4 end

  fun advanceOne st =
    let
      val {runners = (r1, r2, r3), batting, ...} = st
      val st1 = if r3 then addRun batting st else st
    in setRunners (true, r1, r2) st1 end

  fun forceOutAt target st =
    let
      val {runners = (r1, r2, r3), batting, ...} = st
      val st1 = if target = HomePlate andalso r3 then addOut (setRunners (r1, r2, false) st) else st
      val st2 = if target = Third andalso r2 then addOut (setRunners (r1, false, r3) st1) else st1
      val st3 = if target = Second andalso r1 then addOut (setRunners (false, r2, r3) st2) else st2
      val st4 = if target = First then addOut st3 else st3
    in st4 end

  fun basePoint 1 = (240.0, 90.0)
    | basePoint 2 = (153.0, 20.0)
    | basePoint 3 = (90.0, 100.0)
    | basePoint _ = (159.0, 165.0)

  fun drawRunner win color p =
    (GL.color win color;
     GL.circle win p 3.0;
     GL.line win p (#1 p, #2 p + 8.0);
     GL.line win (#1 p, #2 p + 8.0) (#1 p - 4.0, #2 p + 14.0);
     GL.line win (#1 p, #2 p + 8.0) (#1 p + 4.0, #2 p + 14.0))

  fun drawFielder win color p =
    (GL.color win color;
     GL.circle win p 3.5;
     GL.rect win (#1 p - 3.0, #2 p + 4.0) (#1 p + 3.0, #2 p + 14.0);
     GL.line win (#1 p - 6.0, #2 p + 8.0) (#1 p + 6.0, #2 p + 8.0))

  fun drawBall win p = (GL.color win white; GL.circle win p 2.0)

  fun drawDiamond win =
    (GL.color win dirt;
     GL.line win (160.0, 100.0) (240.0, 100.0);
     GL.line win (240.0, 100.0) (160.0, 30.0);
     GL.line win (160.0, 30.0) (80.0, 100.0);
     GL.line win (80.0, 100.0) (160.0, 180.0);
     GL.line win (160.0, 180.0) (240.0, 100.0);
     GL.line win (160.0, 180.0) (80.0, 100.0);
     List.app (fn p => (GL.color win white; GL.rect win (#1 p - 4.0, #2 p - 2.0) (#1 p + 4.0, #2 p + 2.0)))
       [(240.0, 100.0), (160.0, 30.0), (80.0, 100.0), (160.0, 180.0)])

  fun drawOutfield win =
    (GL.color win green;
     GL.line win (10.0, 180.0) (80.0, 100.0);
     GL.line win (80.0, 100.0) (160.0, 30.0);
     GL.line win (160.0, 30.0) (240.0, 100.0);
     GL.line win (240.0, 100.0) (310.0, 180.0))

  fun drawSwingFrame win color frame =
    let
      val batTop = case frame of
          0 => (149.0, 151.0)
        | 1 => (141.0, 150.0)
        | 2 => (132.0, 157.0)
        | 3 => (145.0, 172.0)
        | 4 => (160.0, 170.0)
        | _ => (166.0, 162.0)
    in
      drawRunner win color (149.0, 165.0);
      GL.color win grey;
      GL.line win (149.0, 160.0) batTop
    end

  fun drawScoreboard win ({inning, outs, strikes, home, visitors, batting, runners, message} : state) =
    let
      val (r1, r2, r3) = runners
      fun mark b = if b then "*" else "-"
    in
      GL.color win white;
      GL.text win (5.0, 12.0) ("INNING: " ^ Int.toString inning ^ "  BATTING: " ^ battingName batting);
      GL.text win (5.0, 25.0) ("HOME: " ^ Int.toString home ^ "  VISITORS: " ^ Int.toString visitors);
      GL.text win (5.0, 38.0) ("STRIKES: " ^ Int.toString strikes ^ "  OUTS: " ^ Int.toString outs ^
                                "  BASES: " ^ mark r1 ^ mark r2 ^ mark r3);
      GL.text win (5.0, 195.0) message
    end

  fun drawField win st =
    let val {batting, runners = (r1, r2, r3), ...} = st
        val offense = if batting = Visitors then blue else red
        val defense = if batting = Visitors then red else blue
    in
      GL.clear win green;
      drawOutfield win;
      drawDiamond win;
      drawFielder win defense (159.0, 89.0);
      List.app (drawFielder win defense) [(94.0, 74.0), (227.0, 80.0), (185.0, 40.0), (134.0, 40.0)];
      drawRunner win offense (149.0, 165.0);
      if r1 then drawRunner win offense (240.0, 90.0) else ();
      if r2 then drawRunner win offense (153.0, 20.0) else ();
      if r3 then drawRunner win offense (90.0, 100.0) else ();
      drawScoreboard win st;
      GL.present win
    end

  fun animateLine win st color fromPt toPt steps drawSprite =
    let
      fun loop i =
        if i > steps then ()
        else
          let
            val t = Real.fromInt i / Real.fromInt steps
            val x = #1 fromPt + (#1 toPt - #1 fromPt) * t
            val y = #2 fromPt + (#2 toPt - #2 fromPt) * t
          in
            drawField win st;
            drawSprite win (x, y);
            GL.present win;
            GL.delayMs frameMs;
            loop (i + 1)
          end
    in loop 0 end

  fun pitchSpeed (GL.Char #"2") = SOME {dy = 0.30, dx = 0.03, bonus = 20}
    | pitchSpeed (GL.Char #"5") = SOME {dy = 0.20, dx = 0.02, bonus = 10}
    | pitchSpeed (GL.Char #"8") = SOME {dy = 0.10, dx = 0.01, bonus = 1}
    | pitchSpeed _ = NONE

  fun laneOf y = if y <= 165.0 then LeftLane else if y <= 170.0 then MiddleLane else RightLane

  fun pseudoRandom limit =
    let val t = Time.toMilliseconds (Time.now ())
    in IntInf.toInt (IntInf.mod (t, IntInf.fromInt limit)) end

  fun chooseResult forcedBunt =
    if forcedBunt then GroundOut
    else if pseudoRandom 500 < 150 then Hit
    else if pseudoRandom 300 < 150 then PopOut
    else GroundOut

  fun waitForPitch win =
    let
      fun loop () =
        case pitchSpeed (GL.pollKey win) of
          SOME p => p
        | NONE => (GL.delayMs 10; loop ())
    in loop () end

  fun playPitch win st =
    let
      val speed = waitForPitch win
      fun ballLoop (x, y, frame) =
        if y >= 180.0 then (addStrike st, NONE)
        else
          let val key = GL.pollKey win
          in
            drawField win st;
            drawBall win (x, y);
            if key = GL.Space orelse key = GL.Char #"b" orelse key = GL.Char #"B" then
              let
                val () = List.app (fn f => (drawField win st; drawSwingFrame win yellow f; drawBall win (x, y); GL.present win; GL.delayMs 35)) [0,1,2,3,4,5]
              in
                if y >= 150.0 then (st, SOME (chooseResult (key = GL.Char #"b" orelse key = GL.Char #"B"), laneOf y, #bonus speed))
                else (addStrike st, NONE)
              end
            else
              (GL.present win; GL.delayMs frameMs; ballLoop (x + #dx speed, y + #dy speed * 8.0, frame + 1))
          end
    in ballLoop (153.0, 87.0, 0) end

  fun laneTarget LeftLane = (55.0, 95.0)
    | laneTarget MiddleLane = (160.0, 55.0)
    | laneTarget RightLane = (265.0, 95.0)

  fun playHomeRun win st lane =
    let
      val feet = Int.max (250, pseudoRandom 500)
      val st1 = scoreAllRunners st
      val msg = "A " ^ Int.toString feet ^ " FT HOME RUN!"
    in
      animateLine win (withMessage msg st1) yellow (159.0, 180.0) (laneTarget lane) 60 drawBall;
      withMessage msg st1
    end

  fun playPopOut win st lane =
    let
      val target = laneTarget lane
      val msg = "POPOUT - CAUGHT"
      val st1 = withMessage msg (addOut st)
    in
      animateLine win st1 yellow (159.0, 180.0) target 50 drawBall;
      st1
    end

  fun applySafeGrounder st = withMessage "GROUND BALL - SAFE" (advanceOne st)

  fun applyFieldersChoice target st =
    withMessage "GROUND BALL - OUT" (forceOutAt target st)

  fun waitForThrow win =
    let
      fun loop () =
        case GL.pollKey win of
          GL.Char #"6" => First
        | GL.Char #"8" => Second
        | GL.Char #"4" => Third
        | GL.Char #"2" => HomePlate
        | _ => (GL.delayMs 10; loop ())
    in loop () end

  fun playGroundOut win st =
    let
      val prompted = withMessage "GROUNDOUT: throw 6=1st, 8=2nd, 4=3rd, 2=home" st
      val () = drawField win prompted
      val target = waitForThrow win
      val targetPt = case target of First => basePoint 1 | Second => basePoint 2 | Third => basePoint 3 | HomePlate => basePoint 0
      val st1 = applyFieldersChoice target prompted
    in
      animateLine win st1 white (159.0, 145.0) targetPt 35 drawBall;
      st1
    end

  fun resolveContact win st (Hit, lane, bonus) =
        if pseudoRandom 1200 < 100 + 50 + (case lane of MiddleLane => 20 | _ => 5) + bonus
        then playHomeRun win st lane
        else (animateLine win st yellow (159.0, 180.0) (laneTarget lane) 50 drawBall;
              withMessage "BASE HIT" (advanceOne st))
    | resolveContact win st (PopOut, lane, _) = playPopOut win st lane
    | resolveContact win st (GroundOut, _, _) = playGroundOut win st

  fun showIntro win =
    let
      fun star n = if n = 0 then () else
        (GL.color win white; GL.circle win (Real.fromInt (pseudoRandom width), Real.fromInt (pseudoRandom height)) 1.0; star (n - 1))
    in
      GL.clear win black;
      GL.color win yellow;
      GL.text win (95.0, 82.0) "ANDY'S BASEBALL";
      GL.text win (108.0, 100.0) "G A M E !";
      GL.color win red;
      GL.rect win (100.0, 70.0) (240.0, 88.0);
      GL.present win;
      GL.delayMs 1000;
      GL.clear win black;
      star 200;
      GL.color win white;
      GL.text win (80.0, 95.0) "I CAN'T SHAKE HIM!";
      GL.present win;
      GL.delayMs 800
    end

  fun gameOver ({inning, home, visitors, ...} : state) = inning > 9 andalso home <> visitors

  fun winner ({home, visitors, ...} : state) =
    if home > visitors then "HOME WINS" else if visitors > home then "VISITORS WIN" else "TIE"

  fun loop win st =
    if gameOver st then
      (GL.clear win black;
       GL.color win white;
       GL.text win (85.0, 90.0) ("FINAL  HOME " ^ Int.toString (#home st) ^ " - VISITORS " ^ Int.toString (#visitors st));
       GL.text win (115.0, 110.0) (winner st);
       GL.present win)
    else
      let
        val st0 = endHalfIfNeeded st
        val () = drawField win (withMessage "Pitch: 8 slow, 5 medium, 2 fast. Space swings. B bunts. Q quits." st0)
        val (afterPitch, contact) = playPitch win st0
        val afterPlay = case contact of NONE => afterPitch | SOME c => resolveContact win afterPitch c
      in
        case GL.pollKey win of
          GL.Char #"q" => ()
        | GL.Char #"Q" => ()
        | GL.Escape => ()
        | GL.Quit => ()
        | _ => loop win (endHalfIfNeeded afterPlay)
      end

  fun run () =
    let val win = GL.openWindow {title = "Andy's Baseball", width = width, height = height}
    in showIntro win; loop win (initialState ()) end
end
