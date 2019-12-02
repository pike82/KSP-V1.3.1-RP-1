// Get Mission Values
LOCAL gui is gui(200).
LOCAL label is gui:ADDLABEL("Enter return height in km").
SET label:STYLE:ALIGN TO "CENTER".
SET label:STYLE:HSTRETCH TO True. // Fill horizontally
LOCAL destvalue is gui:ADDTEXTFIELD("150").
SET destvalue:STYLE:ALIGN TO "CENTER".
SET destvalue:STYLE:HSTRETCH TO True. // Fill horizontally
set destvalue:style:width to 300.
set destvalue:style:height to 18.
// Show the GUI.
gui:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
    set destvalue:onconfirm to { 
		parameter val.
		set val to val:tonumber(0).
		set returnheight to val*1000.
		SET isDone TO TRUE.
	}.
	WAIT 0.5.
}
gui:HIDE().
Print "Will return at: " + returnheight + "m". 
Local sv_ClearanceHeight is 10. 
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
Print "Checking thrust".
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
Wait until Stage:Ready.
STAGE. // Relase Clamps
PRINT "Lift off!!".
wait 5.
Unlock Steering.
//used for return flights
Until EngineStartTime + 120 < time:seconds{
	if ((ship:verticalspeed < 0) and (ship:altitude < 2000)){
		wait 1.
		Local P is SHIP:PARTSNAMED(core:part:Name)[0].
		Local M is P:GETMODULE("ModuleRangeSafety").
		M:DOEVENT("Range Safety").
	}
	if ship:apoapsis > returnheight {
		Lock Throttle to 0.
	}
	wait 5.
} 
stage.
Lock Throttle to 0.
Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
until ALT:RADAR < 5000{
	Wait 2.
}
for RealChute in ship:modulesNamed("RealChuteModule") {
	RealChute:doevent("arm parachute").
	Print "Parchute armed enabled.".
}