//Prelaunch
Wait 1. 
PRINT "Prelaunch.".
Lock Throttle to 1.
SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
LOCK STEERING TO r(up:pitch,up:yaw,facing:roll). 
//Liftoff
STAGE. //Ignite main engines
Print "Starting engines".
Local EngineStartTime is TIME:SECONDS.
Local MaxEngineThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. 
FOR eng IN engList {  
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngineThrust. 
	}
}
Print "Checking thrust ok".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngineThrust{
	Set CurrEngineThrust to 0.
	FOR eng IN engList { 
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start up Failed...Making Safe".
		Shutdown. //ends the script
	}
}
Print "Releasing Clamps".
Wait until Stage:Ready . 
wait 2.
STAGE. // Relase Clamps
PRINT "Lift off!!".
wait 5.
Unlock Steering. //keeping this makes the wings try to provide input
//used for non-return flights
Local engstop is false.
Until ((ship:verticalspeed < 0) and (ship:altitude < 2000)){
	if (ship:AIRSPEED > 300) and (engstop = false){
		FOR eng IN engList { 
			if engstop = false{
				eng:shutdown.
				Set engstop to true.
				Break.
			}
		}
	}
	wait 2.	
}
wait 1.
Local P is SHIP:PARTSNAMED(core:part:Name)[0].
Local M is P:GETMODULE("ModuleRangeSafety").
M:DOEVENT("Range Safety").