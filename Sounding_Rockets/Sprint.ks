// Get Mission Values
LOCAL gui is gui(200).
LOCAL label is gui:ADDLABEL("Destruct height in km").
SET label:STYLE:ALIGN TO "CENTER".
SET label:STYLE:HSTRETCH TO True. 
LOCAL destvalue is gui:ADDTEXTFIELD("").
SET destvalue:STYLE:ALIGN TO "CENTER".
SET destvalue:STYLE:HSTRETCH TO True. 
set destvalue:style:width to 300.
set destvalue:style:height to 18.
// Show the GUI.
gui:SHOW().
LOCAL isDone IS FALSE.
UNTIL isDone {
    set destvalue:onconfirm to { 
		parameter val.
		set val to val:tonumber(0).
		set destructheight to val*1000.
		SET isDone TO TRUE.
	}.
	WAIT 0.5.
}
gui:HIDE().
Print "Will destruct at: " + destructheight + "m". 
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
Local MaxEngThrust is 0. 
Local englist is List().
List Engines.
LIST ENGINES IN engList. 
FOR eng IN engList { 
	Print "eng:STAGE:" + eng:STAGE.
	Print STAGE:NUMBER.
	IF eng:STAGE >= STAGE:NUMBER { 
		SET MaxEngThrust TO MaxEngThrust + eng:MAXTHRUST. 
		Print "Stage Full Engine Thrust:" + MaxEngThrust. 
	}
}
Print "Checking thrust".
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
until CurrEngineThrust > 0.99*MaxEngThrust{ 
	Set CurrEngineThrust to 0.
	FOR eng IN engList {  
		IF eng:STAGE >= STAGE:NUMBER { 
			SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
		}
	}
	if (TIME:SECONDS - EngineStartTime) > 5 {
		Lock Throttle to 0.
		Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
		Print "Engine Start Failed...Making Safe".
		Shutdown. //ends the script
	}
}
wait 0.2.
Print "Releasing Clamps".
Wait until Stage:Ready . 
STAGE. // Relase Clamps
PRINT "Lift off!!".
wait 5.
Unlock Steering. 
Print ship:altitude.
Print ship:verticalspeed.
Until ship:altitude > destructheight or ((ship:verticalspeed < 0) and (ship:altitude < 2000)){
	wait 2.	
}
wait 0.1.
Local P is SHIP:PARTSNAMED(core:part:Name)[0].
Local M is P:GETMODULE("ModuleRangeSafety").
M:DOEVENT("Range Safety").
KUniverse:PAUSE().