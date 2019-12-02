//Prelaunch

//Pre IGM parameters
Global f10 is -22.6858.
Global f11 is 3.1212744023.
Global f12 is -0.1596859743.
Global f13 is 0.0034734047.
Global f14 is -0.0000303768.

Global f20 is -12.7889096425.
Global f21 is 1.1572357155.
Global f22 is -0.0370373199.
Global f23 is 0.0003100451.
Global f24 is -0.0000009124.

Global f30 is 267.8272438978.
Global f31 is -10.4559319501.
Global f32 is 0.1425603089.
Global f33 is -0.0009251725.
Global f34 is 0.0000022968.

Global f40 is -241.4732279128.
Global f41 is 7.4940917776.
Global f42 is -0.098022488.
Global f43 is 0.0005266595.
Global f44 is -0.0000010266.

Global tS1 is 35.0.
Global tS2 is 80.0.
Global tS3 is 115.0.
Global dtf is 0.

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to f_f1_thrust().

Set config:IPU to 1000.

//SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
//Wait for launch window //16/07/1969 at 13:32:00 GMT 18 197 13 22 00  which is apollo 11
Set vYear to 7.
Set vDay to 21.
Set vHour to 12.
Set vMinute to 27.
Set vSecond to 35.

Until TIME:YEAR >= vYear and TIME:DAY >= vDay and Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= (vSecond-17) { // 
    Clearscreen.
    Print TIME:YEAR.
    Print TIME:DAY.
    Print Time:CLOCK.
    Wait 0.001.
}
Print "Mission start".
Print ship:mass.
Print ship:drymass.

// Apollo 11 72.058 azimuth heading
Set sv_intAzimith to 72.058. 
Set basetime to time:seconds + 17.


//Ignition starts 8.9 seconds prior to lift off.
wait until Time:seconds > (basetime-8.9).   
Print "Starting engines".
Local MaxEngineThrust is 0. 
Local englist is List().
Local RO_Engine_offset is 2.55. // to allow for RO engine startup delay compared to real engines

LIST ENGINES IN engList. //Get List of Engines in the vessel
//no5 -6.4
wait until Time:seconds > (basetime-(6.4+RO_Engine_offset)).   
FOR eng IN engList { 
    IF eng:TAG ="F1-C" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-C: Active".  
    }
}
//no1/3 -6.1
wait until Time:seconds > (basetime-(6.1+RO_Engine_offset)). 
FOR eng IN engList { 
    IF eng:TAG ="F1-1" or eng:TAG ="F1-3" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-1/3: Active".  
    }
}
//no4/2 -6.0/-5.9
wait until Time:seconds > (basetime-(6.0+RO_Engine_offset)). 
FOR eng IN engList { 
    IF eng:TAG ="F1-4" or eng:TAG ="F1-2" { 
        eng:activate.
        SET MaxEngineThrust TO MaxEngineThrust + eng:MAXTHRUST. 
        Print "F1-2/4: Active".  
    }
}
STAGE. // Engine stage as activate doesn't stage
Print "Checking thrust ok".
Print f_f1_thrust().
Local CurrEngineThrust is 0.
Local EngineStartFalied is False.
Local EngineStartTime is TIME:SECONDS.
until CurrEngineThrust > 0.95*MaxEngineThrust{ 
    Set CurrEngineThrust to 0.
    FOR eng IN engList { 
        IF eng:IGNITION ="true"{ 
            SET CurrEngineThrust TO CurrEngineThrust + eng:THRUST. 
        }
    }
    //Print CurrEngineThrust.
    
    if Time:seconds > (basetime-0.0) {
        Print time:seconds.
        Lock Throttle to 0.
        Set SHIP:CONTROL:PILOTMAINTHROTTLE TO 0.
        Print "Engine Start up Failed...Making Safe".
        Shutdown. //ends the script
    }
}
//all thrust ok -1.6
Print "Thrust ok: " + (basetime - time:seconds).
Print f_f1_thrust().
Print time:seconds.

Global INU_Zero is r(up:pitch,up:yaw,facing:roll) + r(0,0,sv_intAzimith-90).
Print INU_Zero.
LOCK STEERING TO INU_Zero. 
Global Base_alt is alt:radar.

wait until Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= vSecond.
Global liftoff is time:seconds.
Print "T0: " + liftoff.
Print ship:mass.
Print ship:drymass.
//0.3 first lift off arms released
wait until Time:seconds > (liftoff + 0.3). 
STAGE. // Relase Clamps
Print "Lift off!! " + (time:seconds - liftoff).
Print ship:mass.
Print ship:drymass.
Print f_f1_thrust().
//0.6 umbilical disconnect
wait until Time:seconds > (liftoff + 0.6). 
Print "Umbilical disconect " + (time:seconds - liftoff).

f_Pre_IGM(sv_intAzimith).

//tilt arrest 160 occurs at the end of the loop
//Set sV to ship:facing:forevector.
//lock steering to lookdirup( sV, ship:facing:topvector ).
Print "Tilt Arrest Enabled "+ (time:seconds - liftoff).

//MECO Staging 161.63
Wait UNTIL (time:seconds > (liftoff + 161.63)).
Lock Throttle to 1.
FOR eng IN engList { 
	IF eng:IGNITION ="true"{ 
		eng:shutdown. 
	}
}
Print "MECO:"+(TIME:SECONDS).
Print "Speed: " + SHIP:AIRSPEED.
Print "Altitude: " + altitude.
Print ship:mass.
Print ship:drymass.

//162.1 Ullage motors
Wait UNTIL time:seconds > (liftoff + 162.1).
Stage.
//162.3 retro motor fire (already build into MECO satge)
Wait UNTIL time:seconds > (liftoff + 162.3).
wait until stage:ready.
Stage. // release S-IC
//163.0 S-II engine start
Wait UNTIL time:seconds > (liftoff + 163.0).
wait until stage:ready.
Stage.
Print "Second Stage Ignition".
Print ship:mass.
Print ship:drymass.

f_Orbit_Steer().

wait 2.
Print "Stage Finshed".
Shutdown. //ends the script
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function f_Pre_IGM {
Parameter Az.

    Local Xy is 0.
    Local Xz is 0.
    Local Xx is 0.

	local t1 is 13.0.
	local t2 is 25.0.
	local t3 is 36.0.
	local t4 is 45.0.
	local t5 is 81.0.
	local t6 is 0.0.

	Local dTTf is 1.
	Local I is 1.
	Local Majorloop is 0.

	local tAR is 160.0.//153 nominal but 160 for the tilt arrest on apollo 11
	Print "Pre-IGM running".
	UNTIL time:seconds > (liftoff + tAR){
		Set tc to (time:seconds - liftoff).
		If (tc - Majorloop) > dTTf {
			Set Majorloop to tc.
			//Print "Major loop:" + (tc - Majorloop).
		}
		If (tc < t1) or (alt:radar < (137 + Base_alt)){
				f_Yaw_Man(Tc).
		}

		If (tc <= t2){
			If (tc <= t6){
				//zero time locked at launch with engine out
			}
		}
		If (tc <= tAR){ f_Pre_IGM_Steer(tc).}

		// Clear tower yaw manuever // 1.7
		If (time:seconds > (liftoff + 1.7)) and (I = 1){
		//LOCK STEERING TO HEADING(90, (88.75)).//1.25 degree yaw at 1 degree per second and ramped back to 0 slowly
			Print "Yaw program start" + (time:seconds - liftoff).
			Print f_f1_thrust().
			Set I to I+1.
		}
		// end yaw program 9.7
		If (time:seconds > (liftoff + 9.7)) and (I = 2){
		// LOCK STEERING TO HEADING(90, 88.75).
			Print "Yaw program end " + (time:seconds - liftoff).
			Set I to I+1.
			Global INU_Zero_ is INU_Zero + r(sv_intAzimith-90,0,0).
		}
		// //Pitch and roll program commences at 13.2 seconds
		If (time:seconds > (liftoff + 13.2)) and (I = 3){
		// LOCK STEERING TO HEADING(sv_intAzimith, 88.75).
			Print "Pitch and roll program " + (time:seconds - liftoff).
			Set I to I+1.
		}
		// ourboard engine CANT 20.6
		// end roll program 31.1
		If (time:seconds > (liftoff + 31.1)) and (I = 4){
			Print "End roll program " + (time:seconds - liftoff).
		// lock pitch to 90 - VANG(SHIP:UP:VECTOR, SHIP:VELOCITY:SURFACE).
		// LOCK STEERING TO heading(sv_intAzimith, pitch).
			SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.
			Set I to I+1.
		}
		// mach 1 66.3
		If (time:seconds > (liftoff + 66.3)) and (I = 5){
			Print "Mach 1 " + (time:seconds - liftoff).
			Print "Speed: " + SHIP:AIRSPEED.
			Print "Altitude: " + altitude.
			Print f_f1_thrust().
			Set I to I+1.
		}
		// Max Q at 83
		If (time:seconds > (liftoff + 83)) and (I = 6){
			Print "Max Q " + (time:seconds - liftoff).
			Print "Max Q: " + SHIP:Q.
			Print "Speed: " + SHIP:AIRSPEED.
			Print "Altitude: " + altitude.
			Print ship:mass.
			Print ship:drymass.
			Print f_f1_thrust().
			Set I to I+1.
		}
		//inboard engine cutoff 135.2
		If (time:seconds > (liftoff + 135.2)) and (I = 7){
			FOR eng IN engList { 
				IF eng:TAG ="F1-C" { 
					eng:shutdown.
				}
			}
			Print "Centre shutdown " + (time:seconds - liftoff).
			Print "Speed: " + SHIP:AIRSPEED.
			Print "Altitude: " + altitude.
			Print ship:mass.
			Print ship:drymass.
			Print f_f1_thrust().
			Set I to I+1.
		}
		wait 0.0001.
	} //end loop
}//End Pre-IGM phase


function f_Yaw_Man{
Parameter tc.
    if 1.0 > tc { LOCK STEERING to INU_Zero + r(0, 0, 90-sv_intAzimith). }.
    if (1.0 <= Tc) and (tc < 8.75) { 
		LOCK STEERING to (INU_Zero + r(1.25, 0, 90-sv_intAzimith)). 
		Set SteeringManager:MAXSTOPPINGTIME to 0.2.	
	}.
    if 8.75 <=tc { 
		LOCK STEERING to INU_Zero. 
		Set SteeringManager:MAXSTOPPINGTIME to 1.
	}.
}

function f_Pre_IGM_Steer{
Parameter tc.
	local Xy is 0.
	//If dtf is 0 { Set dtf to tc.}//fix up for engine out freeze.
	Local dtcf is (tc-dtf).//tc is time from liftoff dtf is period of frozen Xy 

    If dTcf < ts1{ Set Xy to f_Pitch_Man(f10, f11, f12, f13, f14, dtcf).}
    If (ts1 <= dtcf) and (dtcf < ts2){ Set Xy to f_Pitch_Man(f20, f21, f22, f23, f24, dtcf).}
    If (ts2 <= dtcf) and (dtcf < ts3){ Set Xy to f_Pitch_Man(f30, f31, f32, f33, f34, dtcf).}
    If ts3 <= dtcf { Set Xy to f_Pitch_Man(f40, f41, f42, f43, f44, dtcf).}
	//LOCK STEERING to INU_Zero + r(0, Xy, 0).
	LOCK STEERING to Heading(sv_intAzimith, 90 + Xy).
	//Print "IGM: " + Xy.
	//Print "dtf:" + dtf.
	//Print "dtcf:" + dtcf.
}

function f_pitch_Man {
Parameter val0, val1, val2, val3, val4, dtcf.
    Return val0 + (val1*dtcf) + (val2*(dtcf^2))+(val3*(dtcf^3))+ (val4*(dtcf^4)).
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function f_Orbit_Steer{
//Used for second stage taking into account the third "end" stage
/////////////////////////////////////////////////////////////////////////////////////
// Credits: Own modifications to:
// http://www.orbiterwiki.org/wiki/Powered_Explicit_Guidance
//With Large assisstance and corrections from:
// https://github.com/Noiredd/PEGAS
// https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19660006073.pdf
// https://amyparent.com/post/automating-rocket-launches/

	local tgt_pe is 191300. //target periapsis
	local tgt_ap is 191300. //target apoapsis
	local tgt_inc is 32.672. //target inclination
	local u is 0.
	local HSL is 8.
	local T1_end is 494.//460.
	local T2_end is 548.
	local T1 is 290.//290.
	local T2 is 54.
	Local T3 is 143.
	local mass_flow1 is 1238.//1238.
	local mass_flow2 is 728.//728.
	local mass_flow3 is 214.
	Local start_mass2 is (218888 + (mass_flow2*T2)). //213888
	Print "start_mass2" +start_mass2.
	local start_mass3 is 166571.

	local s_Ve1 is 4169.23.
	local s_Ve2 is 4198.
	local s_Ve3 is 4205.

	local tau1 is 667.
	local tau2 is 347.
	local tau3 is 777.

	local Thrust2 is s_Ve2 * mass_flow2.
	Print Thrust2.
	local Thrust3 is s_Ve3 * mass_flow3.
	Print Thrust3.

    local A1 is -0.3.//starting peg variable
    local B1 is 0. //starting peg variable
    local C1 is 0.1. //starting peg variable

    local A2 is -0.15. //starting peg variable
    local B2 is 0. //starting peg variable
    local C2 is 0.1. //starting peg variable

	local A3 is 0. //starting peg variable
    local B3 is 0. //starting peg variable
    local C3 is 0.1. //starting peg variable

    local converged is 1. // used by convergence checker
    local delta is 0. //time between peg loops
	local peg_step is 1.0.//time between each calcuation check
	local A is 0.
	local B is 0.
	local C is 0.
	local s_pitch is 0.

	local dA1 is 0.
	local dA2 is 0.
	local dB1 is 0.
	local dB2 is 0.

	//values setup
    set ra to body:radius + tgt_ap. //full Ap
    set rp to body:radius + tgt_pe. //full pe
    local sma is (ra+rp)/2. //sma
    local ecc is (ra-rp)/(ra+rp). //eccentricity
    local vp is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis
	if u = 0 {
    	set tgt_vy to 0. // this is the split of the target velocity at the point in time
    	set tgt_vx to vp. // this is the split of the target velocity at the point in time (should be zero for u = 0)
		set rc to rp. // this is the target radius based on the desire true anomoly
	}else{
		set rc to (sma*(1-ecc^2))/(1+ecc*cos(u)). // this is the target radius based on the desire true anomoly
    	local vc is sqrt((vp^2) + 2*body:mu*((1/rc)-(1/rp))). // this is the target velocity at the target radius (if u is zero this will equal vp)
    	local uc is 90 - arcsin((rp*vp)/(rc*vc)). // this is the direction vector of the target velocity
    	set tgt_vy to vc*sin(uc). // this is the split of the target velocity at the point in time
    	set tgt_vx to vc*cos(uc). // this is the split of the target velocity at the point in time (should be zero for u = 0)
	}

    // Define target position and velocities

	local tgt_r is rc.
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. // target angular momentum. This is the velocity represented as energy at a point made up of the x and y components.
	Local tgt_w is sqrt((tgt_vx^2) + (tgt_vy^2)) / (tgt_r).

	Local I is 0.
	Local J is 0.
	local tau_lock is false.

	//192.3 seperate aft interstage
	Wait UNTIL time:seconds > (liftoff + 192.3).
	Stage.
	Print "S-II aft interstage release".
	Print ship:mass.
	Print ship:drymass.
	//197.9 LET release
	Wait UNTIL time:seconds > (liftoff + 197.9).
	Stage.
	Print "S-LET release".
	Print ship:mass.
	Print ship:drymass.
	SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.0.


    local rTcur is (time:seconds - liftoff).
	local last is (time:seconds - liftoff).
	local lastM is (time:seconds - liftoff).
    local s_r is ship:orbit:body:distance.
	local s_acc is ship:AVAILABLETHRUST/ship:mass.
	local s_vy is ship:verticalspeed.
	local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
	local w is s_vx / s_r.
	local s_ve is f_Vel_Exhaust().
	local tau is s_ve/s_acc. //time to burn ship if all propellant
	local w_T1 is w*1.01. //first guess at tgt_w which is actually the current w plus 1%.
	local rT1 is s_r + ((tgt_r-s_r)*0.75). //first guess at rT1
	local w_T2 is w_T1*1.01. 
	local rT2 is s_r + ((tgt_r-s_r)*0.9). //first guess at rT2
	local w_T3 is w_T2*1.01. 
	local rT3 is tgt_r. //first guess at rT3

	Clearscreen.
	Print "IGM Phase 1" .//AT (0,1).
    Print "Action: IGM Convergence loop" .//AT (0,2).
	//Loop through updating the parameters until the break condition is met
    until false {

		//Set time periods
        set rTcur to (time:seconds - liftoff).
		Set DeltaM to rTcur - LastM. // time since last minor calc loop
		set delta to rTcur - last. // time since last major calculation
		set LastM to rTcur.

		set A to A + (B*DeltaM).
		if T1> 0 {
			Set T1 to T1 - DeltaM.
			//Set T1 to T1-delta. //found this is not accurate enough to track T1 count down
			Set A1 to A1 + (B1 * DeltaM).
		}
		if (T2> 0)  and (T1 = 0){
			Set T2 to T2 - DeltaM.
			//Set T2 to T2-delta. /found this is not accurate enough to track T2 count down
			Set A2 to A2 + (B2 * DeltaM).
		}
		if T3> 0 and (T2 = 0){
			Set T3 to T3 - DeltaM.
			Set A3 to A3 + (B3 * DeltaM).
		}

		// collect current ship parameters
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		set w to s_vx / s_r.
		set s_ve to f_Vel_Exhaust().
		set tau to s_ve/s_acc.

		If tau_lock = false{
			Set tau to s_ve/s_acc.
		} else{
			Set tau to T1*(5/4).
		}

		if T1 >0 {
			set s_Ve1 to s_ve.
			set tau1 to tau.
		} 

		if (T2> 0)  and (T1 = 0){
			set s_Ve2 to s_ve.
			set tau2 to tau.
		}
		if T3> 0 and (T2 = 0){
			set s_Ve3 to s_ve.
			set tau3 to tau.
		}

		//204.1 IGM phase 1 (Iterative guidance mode)
		if (time:seconds < (liftoff + 204.1)){
			// pitch hold until this time
		}
		//460.6 Centre engine cutout
		if (time:seconds > (liftoff + 460.6)) and (I = 0){
			Local englist is List().
			LIST ENGINES IN engList.
			FOR eng IN engList { 
				IF eng:TAG ="J2-C" { 
					eng:shutdown.
				}
			}
			//Print "Centre shutdown " + (time:seconds - liftoff).
			//f_clearLine(2).
			Print "Action: CECO" .//AT (0,2).
			Print "Speed: " + SHIP:AIRSPEED .//AT (0,3).
			Print "Altitude: " + altitude .//AT (0,4).
			Print "Mass: " +ship:mass .//AT (0,5).
			Print "Dry Mass: " +ship:drymass .//AT (0,6).
			Set I to I+1.
			Set T1 to 38.
			//Set T1_end to 494. //one off increase to allow for the centre engine shutdown
		}

		// 494.4 IGM phase 2 and tau mode
		if (time:seconds > (liftoff + 494.4)) and (I = 1){
			f_clearLine(1).
			Print "IGM Phase 2 and Tau mode" AT (0,1).
			Set tau_lock to true.
			Set I to I+1.
			Set T1 to 0. //end phase 1
		}

		//498 High(5.5) to low MRS(4.34) command 
		if (time:seconds > (liftoff + 498)) and (I = 2){
			LIST ENGINES IN engList.
			FOR eng IN engList { 
				IF eng:TAG ="J-2" { 
					Local M is eng:GETMODULE("EMRController").
					M:DOAction("change EMR mode", true).
				}
			}
			//Print "Mixture Ratio Shift" + (time:seconds - liftoff).
			//f_clearLine(2).
			Print "Action: CECO" .//AT (0,2).
			Print "Speed: " + SHIP:AIRSPEED .//AT (0,3).
			Print "Altitude: " + altitude .//AT (0,4).
			Print "Mass: " +ship:mass .//AT (0,5).
			Print "Dry Mass: " +ship:drymass .//AT (0,6).
			Set I to I+1.
		}
		// 504.2 end tau mode
		if (time:seconds > (liftoff + 494.4)) and (I = 3){
			//f_clearLine(1).
			Print "IGM Phase 2" AT (0,1).
			Set tau_lock to false.
			Set I to I+1.

		}

		// 548.22 engine cutout and IGM phase 3
		if (time:seconds > (liftoff + 547.5)) and (I = 4){
			Set T2 to 0. //end phase 2
			FOR eng IN engList { 
				IF eng:IGNITION ="true"{ 
					eng:shutdown. 
				}
			}
		
			Print "ECO:"+(TIME:SECONDS).
			Print "Speed: " + SHIP:AIRSPEED.
			Print "Altitude: " + altitude.
			Print ship:mass.
			Print ship:drymass.
			Stage.
			// 548.9 ullage motors
			Wait UNTIL time:seconds > (liftoff + 548.9).
			wait until stage:ready.
			Stage.
			//549.0 seperation
			Wait UNTIL time:seconds > (liftoff + 549.0).
			wait until stage:ready.
			Stage.
			//549.2 engine start
			Wait UNTIL time:seconds > (liftoff + 549.2).
			wait until stage:ready.
			Stage.
			RCS on.
			FOR eng IN engList { 
				IF eng:TAG ="APS" { 
					eng:shutdown.
				}
				IF eng:TAG ="J2-1" { 
					Local M is eng:GETMODULE("EMRController").
					Print M:GETFIELD("current EMR").
					M:SETFIELD("current EMR",4.93).
					M:DOEvent("Show EMR Controller").
				}
			}
			Set I to I+1.
			SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.
		}////

		//555.6 tau mode

		//561.0 ullage case jettison
		//Wait UNTIL time:seconds > (liftoff + 561.0).
		//Stage.
		//562.4 end of tau mode
		

		//665.2 begin terminal guidance

		//691.6 end IGM phase 3
		if (time:seconds > (liftoff + 699.34)) and (I = 5){
			break. //leave loop
		}
		
		//////////PEG loop//////////////////////
    	If (delta >= peg_step) {  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle calculations
			Set last to (time:seconds - liftoff).//reset last
			Print "######Convergence Step######".// AT (0,9).
			Print "Delta" + delta.
			//Set delta to 0.

			// if T1> 0 {
			// 	Set T1 to T1_end - rTcur.
			// 	//Set T1 to T1-delta. //found this is not accurate enough to track T1 count down
			// 	Set A1 to A1 + (B1 * delta).
			// }
			// if (T2> 0)  and (T1 = 0){
			// 	Set T2 to T2_end - rTcur.
			// 	//Set T2 to T2-delta. /found this is not accurate enough to track T2 count down
			// 	Set A2 to A2 + (B2 * delta).
			// }
			// if T3> 0 and (T2 = 0){
			// 	Set T3 to T3-delta.
			// 	Set A3 to A3 + (B3 * delta).
			// }

			/// determine peg states
			local peg_solved is f_PEG(A1, B1, T1, rT1, w_T1, s_ve1, tau1, tgt_vy, tgt_vx, tgt_r, tgt_w, mass_flow1,  
										A2, B2, T2, rT2, w_T2, s_ve2, tau2, start_mass2, mass_flow2, Thrust2,
										A3, B3, T3, rT3, w_T3, s_ve3, tau3, start_mass3, mass_flow3, Thrust3,
										dA1, dA2, dB1, dB2).

			set A1 to peg_solved[0].
			set B1 to peg_solved[1].
			set T1_new to peg_solved[2].
			set rT1 to peg_solved[3].
			set w_T1_new to peg_solved[4].

			set A2 to peg_solved[5].
			set B2 to peg_solved[6].
			set T2_new to peg_solved[7].
			set rT2 to peg_solved[8].
			set w_T2_new to peg_solved[9].

			set A3 to peg_solved[10].
			set B3 to peg_solved[11].
			set T3_new to peg_solved[12].
			set rT3 to peg_solved[13].
			set w_T3_new to peg_solved[14].

			set dA1 to peg_solved[15].
			set dA2 to peg_solved[16].
			set dB1 to peg_solved[17].
			set dB2 to peg_solved[18].

			//local tgt_vy1 is 73.66.
			//local tgt_vx1 is 6918.6.
			//local tgt_r1 is 6559612.

		
			if T1 >0 {
				Print "Conv Step percent: " + f_Tol (w_T1, w_T1_new, abs(w_T1*0.0015)) .//AT (0,11).
				if f_Tol (w_T1, w_T1_new, abs(w_T1*0.0015)) { // check if old w_T1 and new w_T1 are converged
					if converged < 0 {
						set converged to converged+1. //(this is done over 3 cycles to ensure the convergence solution selected is accurate enough over ten ship location updates rather than relying on only one convergence solution to enter a closed loop)
						//f_clearLine(10).
						//Print "Convergence step +1" .//AT (0,10).
					} 
					if converged > -1 {
						set converged to 1.
						//f_clearLine(10).
						//Print("closed loop enabled") .//AT (0,10).
					}
				} 
				set w_T1 to w_T1_new.
				set w_T2 to w_T2_new.
				set w_T3 to w_T3_new.
				set T3 to T3_new.
				set A to A1.
				set B to B1.
			} 

			if (T2> 0)  and (T1 = 0){
				set w_T2 to w_T2_new.
				set w_T3 to w_T3_new.
				set T3 to T3_new.
				set A to A2.
				set B to B2.
			}
			if T3> 0 and (T2 = 0){

				if(T3_new <= HSL) { // HSL check time to go is above (8 seconds is apollo 11 e3 value see para 4.1.7), below this the solution starts to become very sensitive and A and B should not longer be re-calculated
					Print "Terminal guidance enabled". 
					// keep constants static
				} Else{
					Print "Non terminal guidance".
					set A to A3.
					set B to B3.
				}
				Set T3 to T3_new.
				set w_T3 to w_T3_new.
			}
				
		}

		
		// Print "Pitch: " + s_pitch .//AT (0,13).
		// Print "T1: " + T1 .//AT (0,14).
		// Print "A1: " + A1 .//AT (0,15).
		// Print "B1: " + B1 .//AT (0,16).

		// Print "tau: " + tau .//AT (0,18).
		// Print "C: " + C .//AT (0,19).

		// Print "Staging w: " + w_T1 .//AT (0,21).
		// Print "Staging height: " + (rT1 - body:radius) .//AT (0,22).
		// Print "Staging vo: " + w_T1 * rT1 .//AT (0,23).
		// Print "Staging Pitch : " + arcsin( max(-0.9, min(A1+C1,0.9)) ) .//AT (0,24).

		// Print "T2: " +T2 .//AT (0,25).
		// Print "A2: " +A2 .//AT (0,26).
		// Print "B2: " +B2 .//AT (0,27).
		// Print "C2: " +C2 .//AT (0,28).

		// Print "delta: " + delta .//AT (0,30).
		// Print "missiontime: " + missiontime .//AT (0,31).

		// Print "CT: " + CT .//AT (0,33).
		// Print "dv2: " + dV2 .//AT (0,34).
		// Print "tau2: " + tau2.//AT (0,35).
		// Print "dA: " + dA .//AT (0,36).
		// Print "dB: " + dB .//AT (0,37).
		//print "converged value" +converged.
		if (converged = 1) and (time:seconds > (liftoff + 204.1)){
			set C to ((body:mu/(s_r^2)) - ((w^2)*s_r))/s_acc.	
			local s_pitch is A + C. //sin pitch at current time.
			set s_pitch to max(-0.707, min(s_pitch, 0.707)). // limit the pitch change to between -45 and 45 degress
			Set s_pitch to arcsin(s_pitch). //covert into degress
			Print "S pitch: " + s_pitch.
			LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
		}
		wait 0.001. 
	}//end of loop

	//Loop through last conditions without PEG calculations
    until false {
		//699.34 ECO command
		if (time:seconds > (liftoff + 699.34)) and (I = 5){
			FOR eng IN engList { 
				IF eng:TAG ="J2-1" { 
					eng:shutdown.
				}
			}
			Print "ECO:"+(TIME:SECONDS).
			Print "Speed: " + SHIP:AIRSPEED.
			Print "Altitude: " + altitude.
			Print ship:mass.
			Print ship:wetmass.
			Print ship:drymass.
			Set I to I+1.
		}

			//699.8 APS ullage command
		if (time:seconds > (liftoff + 699.34)) and (I = 6){
			FOR eng IN engList { 
				IF eng:TAG ="APS" { 
					eng:activate.
				}
			}
			Set I to I+1.
		}
		//709.34 Orbit instertion

		//719.3 horitzontal move to
		if (time:seconds > (liftoff + 719.3)) and (I = 7){
			//786.5 ullage engine turn off
			Wait until ship:orbit:eccentricity < 0.01 or time:seconds > (liftoff + 786.5).
			FOR eng IN engList { 
				IF eng:TAG ="APS" { 
					eng:shutdown.
				}
			}
			break.
		}
	}//end loop

} // end of function
	

function f_PEG {
    parameter A1.
    parameter B1.
    parameter T1.
	parameter rT1.
	parameter w_T1. 
	parameter s_ve1.
	parameter tau1.
	parameter tgt_vy. // orbit "target" vertical velocity
	parameter tgt_vx. // orbit "target" horizontal velocity
	parameter tgt_r. // orbit "target" radius
	parameter tgt_w.
	parameter mass_flow1 is 0.

    parameter A2 is 0.
    parameter B2 is 0.
    parameter T2 is 0.
	parameter rT2 is 0.
	parameter w_T2 is 0. 
	parameter s_ve2 is 0.
	parameter tau2 is 1.
	parameter start_mass2 is 0.
	parameter mass_flow2 is 0.
	parameter Thrust2 is 0.

	parameter A3 is 0.
    parameter B3 is 0.
    parameter T3 is 0.
	parameter rT3 is 0.
	parameter w_T3 is 0. 
	parameter s_ve3 is 0.
	parameter tau3 is 1.
	parameter start_mass3 is 0.
	parameter mass_flow3 is 0.
	parameter Thrust3 is 0.

	parameter dA1 is 0.
	parameter dA2 is 0.
	parameter dB1 is 0.
	parameter dB2 is 0.

	// read current stage and position values

	local s_vy is ship:verticalspeed.
	local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
	local s_r is ship:orbit:body:distance.
	local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	local w is s_vx /s_r.
	//local w is sqrt((s_vx^2) + (s_vy^2)) / (s_r).
	local h0 is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. //current angular momentum
	Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. //target angular momentum

	local s_acc_2 is 0.
	local s_acc_3 is 0.

	local s_acc_end_1 is 0.
	local s_acc_end_2 is 0.
	local s_acc_end_3 is 0.

	local rdotT2 is 0.
	local rdotT3 is 0.


	/// determine bn and cn stages

		local L1 is f_bcn(s_ve1, tau1, T1).

		local bb01 is L1[0].
		local bb11 is L1[1].
		local bb21 is L1[2].
		local cc01 is L1[3].
		local cc11 is L1[4].
		local cc21 is L1[5].

		local L2 is f_bcn(s_ve2, tau2, T2).

		local bb02 is L2[0].
		local bb12 is L2[1].
		local bb22 is L2[2].
		local cc02 is L2[3].
		local cc12 is L2[4].
		local cc22 is L2[5].

		local L3 is f_bcn(s_ve3, tau3, T3).

		local bb03 is L3[0].
		local bb13 is L3[1].
		local bb23 is L3[2].
		local cc03 is L3[3].
		local cc13 is L3[4].
		local cc23 is L3[5].

		Print "Checks check:".
		Print "bb01: " + bb01.
		Print "bb11: " + bb11.
		Print "s_r: "+s_r.
		Print "s_vy: " + s_vy.
		Print "tgt_r: "+tgt_r.
		Print "T1: " + T1.
		Print "A1: " + A1.
		Print "B1: " + B1.
		Print "rT1: " + rT1.
		Print "dA1: " + dA1.
		Print "dB1: " + dB1.
		Print "T2: " + T2.
		Print "A2: " + A2.
		Print "B2: " + B2.
		Print "rT2: " + rT2.
		Print "dA2: " + dA2.
		Print "dB2: " + dB2.
		Print "T3: " + T3.
		Print "A3: " + A3.
		Print "B3: " + B3.
		Print "rT3: " + rT3.
		Print "Height: "+ (rT1 - body:radius).
		Print "(s_vy*T1)" + (s_vy*T1).
		Print "(cc01 * A1)" + (cc01 * A1).
		Print "(cc11*B1)" + (cc11*B1).
		Print "combined: " + ((s_vy*T1)+(cc01 * A1)+(cc11*B1)).

		//get future stage parameters
//first of three
		if T1 > 0{

			set s_acc_end_1 to ship:AVAILABLETHRUST/ (ship:mass - ((mass_flow1/1000)*T1)).//*(4/5). //(4/5) for the engine out condition
			Print s_acc_end_1.
			set rdotT1 to s_vy + bb01*A1 + bb11*B1. //vertical speed at staging
			set rT1 to s_r + (s_vy*T1) + (cc01*A1) + (cc11*B1). // vertical radius at staging
			Print "rdotT1: "+rdotT1.
			Print "rT1: "+ rT1.
			Print "sVe1: " + s_ve1.
			//apply boundaries on results
			if rT1 > tgt_r {Set rT1 to (tgt_r-30000).}
			if rT1 < s_r {Set rT1 to tgt_r.}

			local L4 is f_end_cond(w, s_r, s_acc, w_T1, rT1, s_acc_end_1, T1, A1, B1). 
			local ft_1 is L4[0].
			local ftdot_1 is L4[1].
			local ftdd_1 is L4[2].
			local dh_T1 to ((s_r + rT1)/2)*( (ft_1*bb01) + (ftdot_1*bb11) + (ftdd_1*bb21) ).
			Set hT1 to dh_T1 + h0.
			local v0_T1 is hT1/rT1.
			//contraint on V0_T1 to less than remaining stage dv
			Print "v0_T1 (1): " + v0_T1.
			Print dh_T1 /rT1.
			// if V0_T1 > (bb01 + sqrt(s_vx^2 + s_vy^2)){
			// 	set V0_T1 to (bb01 + sqrt(s_vx^2 + s_vy^2)).
			// 	set hT1 to V0_T1*rT1.
			// }
			//Print tau. 
			Print "T1: " + T1.
			//Print L1.
			//Print L4.
			Print "v0_T1: " + v0_T1.
			Print "rT1: " + rT1.
			Set w_T1 to sqrt((v0_T1^2) - (rdotT1^2))/rT1.

			set mean_r to (s_r + rT1)/2.
			local dv_gain is dh_T1/mean_r.
			Print "dv gain to T1: " + dv_gain.

			if T2 = 0 { // if only single stage to orbit
				Set T1 to tau*(1 - constant:e ^ (-dv_gain/s_ve)).
			}
			if T2 > 0 {
				set s_acc_2 to Thrust2/start_mass2.
				set s_acc_end_2 to Thrust2/ (start_mass2 -((mass_flow2/1000)*T2)).

				//set rdotT2 to rdotT1 +bb02*A2 + bb12*B2. //vertical speed at staging
				set rdot2 to s_vy + bb01*A1 + ((bb11+(bb01*0))*B1) +(bb01*0 + bb01*0*0 +bb11*0).
				set rT2 to s_r + (s_vy*T1) + ((cc01 + 0)*A1) + ((cc11+(cc01*0+0*T1+0*T1*0))*B1) + ( (cc01*0) +(cc11*0) + (0*T1*0 + 0*0*T1*0 + 0*T1*0 + cc01*0*0)  ) . // vertical radius at staging
				Print "rdotT2: "+rdotT2.
				Print "rT2: "+ rT2.
				//apply boundaries on results
				//if rT2 > tgt_r{ Set rT2 to tgt_r.}
				//if rT2 < rT1 { Set rT2 to rT1.}

				Local L5 is f_end_cond(w_T1, rT1, s_acc_2, w_T2, rT2, s_acc_end_2, T2, A2, B2). 
				local ft_2 is L5[0].
				local ftdot_2 is L5[1].
				local ftdd_2 is L5[2].
				local dh_T2 to ((rT1 + rT2)/2)*( (ft_2*bb02) + (ftdot_2*bb12) + (ftdd_2*bb22) ).
				Set hT2 to dh_T2 + hT1.
				local v0_T2 is hT2/rT2.
				//contraint on V0_T1 to less than remaining stage dv
				//Print "v0_T2 (1): " + v0_T2.
				// if V0_T2 > (bb02 + V0_T1){
				// 	set V0_T2 to (bb02 + V0_T1).
				// 	set hT2 to V0_T2*rT2.
				// }
				// if V0_T2 < (V0_T1){
				// 	set V0_T2 to (V0_T1).
				// 	set hT2 to V0_T2*rT2.
				// }
				//Print L5.
				print "v0_T2 " + v0_T2.
				print "rT2" + rT2.
				Set w_T2 to sqrt((v0_T2^2) - (rdotT2^2))/rT2.

				set mean_r to (rT2 + rT1)/2.
				local dv_gain is dh_T2/mean_r.
				Print "dv gain to T1 to T2: " + dv_gain.

				if T3 = 0{ // if only two stage to orbit
				 	Set T2 to tau2*(1 - constant:e ^ (-dv_gain/s_ve2)).
					//set T2 boundaries
					if T2 <0 {Set T2 to 2.}
				}

				if T3 > 0 {

					set s_acc_3 to Thrust3/start_mass3.
					set s_acc_end_3 to Thrust3/ (start_mass3 -((mass_flow3/1000)*T3)).
					//l=2, k=1
					set rdotT3 to s_vy + bb02*A1 + ( ((bb11+(bb01*0)) +(bb12 + (bb02*T1)))*B1) + (bb01*0 + bb01*0*0 +bb11*0) + ((bb02*dA1) + (bb01*T1*0) + (bb12*dB1)) . //vertical speed at staging
					set rT3 to s_r + (s_vy*(T1+T2)) + (((cc01 + T1*0) + ( cc02+(T2*bb01)))*A1) + (((cc11+(cc01*0+0*T1+0*T1*0)) +  ( (cc02*T1) + (bb11*T2) + (bb01*T2*0)  )  )*B1) + ( ((cc01*0) +(cc11*0) + (0*T1*0 + 0*0*T1*0 + 0*T1*0 + cc01*0*0)) + ((cc02*dA1) +(cc12*dB1) + (bb01*T2*0 + bb01*0*T2*0 + bb11*T2*0 + cc02*T1*0))  ). // vertical radius at staging
					Print "rdotT3: "+rdotT3.
					Print "rT3: "+ rT3.
					//apply boundaries on results
					//if rT3 > tgt_r{ Set rT3 to tgt_r.}
					//if rT3 < rT2 {Set rT3 to rT2.}

					Local L6 is f_end_cond(w_T2, rT2, s_acc_3, w_T3, rT3, s_acc_end_3, T3, A3, B3). 
					local ft_3 is L6[0].
					local ftdot_3 is L6[1].
					local ftdd_3 is L6[2].
					//local dh_T3 to ((rT2 + rT3)/2)*( (ft_3*bb03) + (ftdot_3*bb13) + (ftdd_2*bb23) ).
					//Set hT3 to dh_T3 + hT2.
					local dh_T3 is tgt_h - hT2. //angular momentum to gain in final stage
					//Set hT3 to dh_T3 + hT2.
					Local hT3 is tgt_h.
					//local v0_T3 is hT3/rT3.
					local v0_T3 is tgt_vx.
					//Print L6.
					print "v0_T3 " + v0_T3.
					local rT3 is tgt_r.
					Print "rT3" + rT3.
					//Set w_T3 to sqrt((v0_T3^2) - (rdotT3^2))/rT3.
					Set w_T3 to tgt_w.

					set mean_r to (rT3 + rT2)/2.
					local dv_T3 is dh_T3/mean_r.
					local tau3 is s_ve3/s_acc_3.
					Set T3 to tau3*(1 - constant:e ^ (-dv_T3/s_ve3)).

					//set T3 boundaries
					if T3 <0 {Set T3 to 2.}

					Print "dv gain T2 to Orbit: " + dv_T3.


				}
			}
		}
///Second of three
		if (T2 > 0)  and (T1 = 0) {

			set s_acc_end_2 to ship:AVAILABLETHRUST/ (ship:mass - ((mass_flow2/1000)*T2)).

			set rdot2 to s_vy + bb01*A2 + ((bb11+(bb01*0))*B2) +(bb01*0 + bb01*0*0 +bb11*0).
			set rT2 to s_r + (s_vy*T2) + ((cc01 + 0)*A2) + ((cc11+(cc01*0+0*T1+0*T1*0))*B2) + ( (cc01*0) +(cc11*0) + (0*T1*0 + 0*0*T1*0 + 0*T1*0 + cc01*0*0)  ) . // vertical radius at staging

			//apply boundaries on results
			if rT2 > tgt_r{ Set rT2 to tgt_r.}
			if rT2 < s_r {Set rT2 to s_r.}

			Local L5 is f_end_cond(w, s_r, s_acc, w_T2, rT2, s_acc_end_2, T2, A2, B2). 
			local ft_2 is L5[0].
			local ftdot_2 is L5[1].
			local ftdd_2 is L5[2].
			local dh_T2 to ((s_r + rT2)/2)*( (ft_2*bb02) + (ftdot_2*bb12) + (ftdd_2*bb22) ).
			Set hT2 to dh_T2 + h0.
			local v0_T2 is hT2/rT2.
			Set w_T2 to sqrt((v0_T2^2) - (rdotT2^2))/rT2.

			set mean_r to (rT2 + s_r)/2.
			local dv_gain is dh_T2/mean_r.
			Print "dv gain T1 to T2: " + dv_gain.
			Print "rT2" + rT2.
			Print "T2: " + T2.

			if T3 = 0{ // if only two stage to orbit
				Set T2 to tau2*(1 - constant:e ^ (-dv_gain/s_ve)).
			}

			if T3 > 0 {

				set s_acc_3 to Thrust3/start_mass3.
				set s_acc_end_3 to Thrust3/ (start_mass3 -((mass_flow3/1000)*T3)).

				set rdotT3 to s_vy + bb02*A2 + ( ((bb11+(bb01*0)) +(bb12 + (bb02*T1)))*B2) + (bb01*0 + bb01*0*0 +bb11*0) + ((bb02*dA2) + (bb01*T1*0) + (bb12*dB2)) . //vertical speed at staging
				set rT3 to s_r + (s_vy*(T3+T2)) + (((cc01 + T1*0) + ( cc02+(T2*bb01)))*A2) + (((cc11+(cc01*0+0*T1+0*T1*0)) +  ( (cc02*T1) + (bb11*T2) + (bb01*T2*0)  )  )*B2) + ( ((cc01*0) +(cc11*0) + (0*T1*0 + 0*0*T1*0 + 0*T1*0 + cc01*0*0)) + ((cc02*dA2) +(cc12*dB2) + (bb01*T2*0 + bb01*0*T2*0 + bb11*T2*0 + cc02*T1*0))  ). // vertical radius at staging

				//apply boundaries on results
				if rT3 > tgt_r{ Set rT3 to tgt_r.}
				if rT3 < rT2 {Set rT3 to rT2.}

				Local L6 is f_end_cond(w_T2, rT2, s_acc_3, w_T3, rT3, s_acc_end_3, T3, A3, B3). 
				local ft_3 is L6[0].
				local ftdot_3 is L6[1].
				local ftdd_3 is L6[2].
				//local dh_T3 to ((rT2 + rT3)/2)*( (ft_3*bb03) + (ftdot_3*bb13) + (ftdd_2*bb23) ).
				//Set hT3 to dh_T3 + hT2.
				local dh_T3 is tgt_h - hT2. //angular momentum to gain in final stage
				Set hT3 to dh_T3 + hT2.
				local v0_T3 is hT3/rT3.
				Set w_T3 to sqrt((v0_T3^2) - (rdotT3^2))/rT3.

				set mean_r to (rT3 + rT2)/2.
				local dv_T3 is dh_T3/mean_r.
				Set T3 to tau3*(1 - constant:e ^ (-dv_T3/s_ve)).

				//set T3 boundaries
				if T3 <0 {Set T3 to 2.}

				Print "dv gain T2 to Orbit: " + dv_T3.
				Print "rT3" + rT3.
			}
		}
//third of three
		if (T3 > 0)  and (T2 = 0) {

			set s_acc_end_3 to ship:AVAILABLETHRUST/ (ship:mass - ((mass_flow3/1000)*T3)).

			set rdotT3 to s_vy + bb02*A3 + ( ((bb11+(bb01*0)) +(bb12 + (bb02*T1)))*B3). //vertical speed at staging
			set rT3 to s_r + (s_vy*(T1+T2+T3)) + (((cc01 + T1*0) + ( cc02+(T2*bb01)))*A3) + (((cc11+(cc01*0+0*T1+0*T1*0)) +  ( (cc02*T1) + (bb11*T2) + (bb01*T2*0)  )  )*B3) + ( ((cc01*0) +(cc11*0) + (0*T1*0 + 0*0*T1*0 + 0*T1*0 + cc01*0*0)) + ((cc02*dA2) +(cc12*dB2) + (bb01*T2*0 + bb01*0*T2*0 + bb11*T2*0 + cc02*T1*0))  ). // vertical radius at staging

			//apply boundaries on results
			if rT3 > tgt_r{ Set rT3 to tgt_r.}
			if rT3 < s_r {Set rT3 to s_r.}

			Local L6 is f_end_cond(w, s_r, s_acc, w_T3, rT3, s_acc_end_3, T3, A3, B3). 
			local ft_3 is L6[0].
			local ftdot_3 is L6[1].
			local ftdd_3 is L6[2].
			//local dh_T3 to (s_r + rT3)/2)*( (ft_3*bb03) + (ftdot_3*bb13) + (ftdd_2*bb23) ).
			//Set hT3 to dh_T3 + h0.
			local dh_T3 is tgt_h - h0. //angular momentum to gain in final stage
			local v0_T3 is hT3/rT3.
			Set w_T3 to sqrt((v0_T3^2) - (rdotT3^2))/rT3.

			set mean_r to (rT3 + s_r)/2.
			local dv_T3 is dh_T3/mean_r.
			set T3 to tau3*(1 - constant:e ^ (-dv_T3/s_ve3)).

			Print "dv gain T2 to Orbit: " + dv_T3.
			Print "rT3" + rT3.
			Print "T3: " + T3.
		}



		//Guidance staging discontinuities

		If (T2>0) and (T1>0){

			set dA1 to ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ).
			//Print "dA1 test:" + dA1.
			set dA1 to dA1 * ( (1/s_acc_end_1) - (1/s_acc_2) ).
			//Print "dA1 test:" + ( (1/s_acc_end_1) - (1/s_acc_2) ).

			set dB1 to - ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ) * ( (1/s_ve1) - (1/s_ve2)  ). 
			//Print "dB1 test:" + dB1.
			set dB1 to dB1 + ( ( (3*(w_T1^2)) - ((2*body:mu)/(rT1^3)) ) *rdotT1* ( (1/s_acc_end_1) - (1/s_acc_2) )  ).
			//Print "dB1 test:" + ( (1/s_acc_end_1) - (1/s_acc_2) ).

			/// Determine A2 and B2

			set A2 to A1 + (dA1 + (B1*T1)). //Aj = A1 + sum dA(l) + B1*T(l) + T(l)*sum(dB(l)) (from l=1 to j-1)
			set B2 to B1 + dB1. //Bj = B1 + sum dB(l) (from l=1 to j-1)
			
			If T3>0{

				set dA2 to ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ).
				set dA2 to dA2 * ( (1/s_acc_end_2) - (1/s_acc_3) ).

				set dB2 to - ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ) * ( (1/s_ve2) - (1/s_ve3)  ). 
				set dB2 to dB2 + ( ( (3*(w_T2^2)) - ((2*body:mu)/(rT2^3)) ) *rdotT2* ( (1/s_acc_end_2) - (1/s_acc_3) )  ).

				/// Determine A3 and B3

				set A3 to A1 + (dA1 + (B1*T1)) + (dA2 + (B1*T2)) + (T2*dB1). //Aj = A1 + sum dA(l) + B1*T(l) + T(l)*sum(dB(l)) (from l=1 to j-1)
				set B3 to B1 + dB1 + dB2. //Bj = B1 + sum dB(l) (from l=1 to j-1)
			
			}
		}

		If (T2>0) and (T1=0){
			Set dA1 to 0.
			Set dB1 to 0.

			set dA2 to ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ).
			set dA2 to dA2 * ( (1/s_acc_end_2) - (1/s_acc_3) ).

			set dB2 to - ( (body:mu/(rT2^2)) - ((w_T2^2)*rT2) ) * ( (1/s_ve2) - (1/s_ve3)  ). 
			set dB2 to dB2 + ( ( (3*(w_T2^2)) - ((2*body:mu)/(rT2^3)) ) *rdotT2* ( (1/s_acc_end_2) - (1/s_acc) )  ).

			/// Determine A3 and B3

			set A3 to A2 + (dA2 + (B2*T2)). 
			set B3 to B3 + dB2.
			
		}

		If (T2=0) and (T1=0){
			Set dA1 to 0.
			Set dB1 to 0.

			Set dA2 to 0.
			Set dB2 to 0.
		}

		//set up matricies
		// L12 // this is just addiotn of the L's
		local mA11 is bb01 + bb02 + bb03. 


		// J12 // addition of J's plus the previous stages time at the current stages bb0
		local mA12 is bb11. //l=1
		set mA12 to mA12 + bb12 + (bb02*T1). //l=2, k=1 
		set mA12 to mA12 + bb13 + (bb03*(T1+T2)). //l=3, k=2

		// S12 addtion of the S's but with the addition of the current stage  
		local mA21 is cc01.
		set mA21 to mA21 + cc02 + (bb01*T2).//l=2, k=1 
		set mA21 to mA21 + cc03 + ((bb02+bb01)*T3). //l=3, k=2
		//set mA21 to cc03 + (T3*(bb01+bb02)). // j=3, l=2  // this is the formula read another way.

		// Q12 Q1 + (J1*T2) + (S2*T1) + Q2
		local mA22 is cc11. 
		set mA22 to mA22 + cc12 + (cc02*T1) + (bb11*T2) +((bb01*T2)*0).//l=2, k=1, i=0 
		set mA22 to mA22 + cc13 + (cc03*T2) + (bb12*T3) + ((bb02*T3)*T1).//l=3, k=2, i=1 *****
		//set mA22 to cc13 + (cc03*T2) + (bb12*T3) + ((bb12*T3)*T1) + (cc03*T1) + (bb11*T3) + ((bb11*T3)*0). // j=3, l=2, k=1  // this is the formula read another way.

    	local mC1 is tgt_vy - s_vy. 
		set mC1 to mC1 - (bb02*dA1) - (bb02*T1*0) - (bb12*dB1). //l=2, k=1, i=0
		set mC1 to mC1 - (bb03*dA2) - (bb03*T2)*dB1 - (bb13*dB2). //l=3, k=2, i=1
		//set mC1 to tgt_vy - s_vy - (bb02*dA1) - (bb02*T1*0) - (bb12*dB1). // j=3, l=2, k=1  // this is the formula read another way.

    	local mC2 is tgt_r - s_r - (s_vy*(T1+T2+T3)).
		set mC2 to mC2 - (cc02*dA1) - (cc12*dB1) - (bb01 *T2 *0) - (bb01*0*T2*0) - (bb11*T2*0) - (cc02*T1*0). //l=2, k=1, i=0, m=-1
		set mC2 to mC2 - (cc03*dA2) - (cc13*dB2) - (bb02*T3*dA1) - (bb02*T1*T3*0) - (bb12*T3*dB1) - (cc03*T2*dB1).//l=3, k=2, i=1, m=0
		//set mC2 to tgt_r - s_r - (s_vy*(T1+T2+T3)) - (cc03*dA2) - (cc13*dB2) - (cc03*dA1) - (cc13*dB1) - (bb01*T3*dA1) - (bb01*T1*T3*0) - (bb11*T3*dB1) - (cc03*T1*dB1). // j=3, l=2, k=1  // this is the formula read another way.

		local peg is f_peg_solve(mA11, mA12, mA21, mA22, mC1, mC2).

		if T1 > 0{
			Set A1 to peg[0].
			Set B1 to peg[1].
			Print "A1 peg"+ A1. 
			Print "B1 peg" + B1.
		}
		if (T2 > 0) and (T1 = 0){
			Set A2 to peg[0].
			Set B2 to peg[1].
			Print "A2 peg"+ A2. 
			Print "B2 peg" + B2.
		}
		if (T2 = 0) and (T1 = 0){
			Set A3 to peg[0].
			Set B3 to peg[1].
			Print "A3 peg"+ A3. 
			Print "B3 peg" + B3.
		}
		wait 0.5.

	Return list(A1, B1,	T1, rT1, w_T1, A2, B2, T2, rT2, w_T2, A3, B3, T3, rT3, w_T3, dA1, dA2, dB1, dB2). 
}

///////////////////////////////////////////////////////////////////////////////////
function f_bcn{
	parameter s_ve.
	parameter tau.
	parameter T.

	local bb0 is -s_ve*(LN(1-(T/tau))).
	//J1
	local bb1 is (bb0 * tau) - (s_ve*T).
	//P1
	local bb2 is (bb1 * tau) - ((s_ve*(T^2))/2).
	//S1
	local cc0 is (bb0*T)-bb1.
	//Q1
	local cc1 is (cc0*tau) - ((s_ve*(T^2))/2).
	//U1
	local cc2 is (cc1*tau) - ((s_ve*(T^3))/6).

	return list(bb0, bb1, bb2, cc0, cc1, cc2).
}
Function f_end_cond{
	parameter start_w.
	parameter start_r.
	parameter start_acc.
	parameter end_w.
	parameter end_r.
	parameter end_acc.
	parameter T_time.
	parameter A.
	parameter B. 

		//Current pitch guidance for horizontal state
		Set C to ((body:mu/(start_r^2)) - ((start_w^2)*start_r))/start_acc. //start portion of vehicle acceleration used to counteract gravity
		local fr is A + C. //sin pitch at start
		local C_end is (body:mu/(end_r^2)) - ((end_w^2)*end_r). //Gravity and centrifugal force term at cutoff
		Set C_end to C_end /end_acc. 
		Set frT to A + (B*T_time) + C_end. //sin pitch at burnout. 
		local frdot is (frT-fr)/T_time. //approximate rate of sin pitch
		local ft is 1 - (frT^2)/2. //cos pitch
		local ftdot is -fr*frdot. //cos pitch speed
		local ftdd is -(frdot^2)/2. //cos pitch acceleration
		
		return list (ft, ftdot, ftdd). 
}
///////////////////////////////////////////////////////////////////////////////////
// Estimate, returns A and B coefficient for guidance
function f_peg_solve {
    parameter mA11.
	parameter mA12. 
	parameter mA21. 
	parameter mA22. 
	parameter mC1. 
	parameter mC2.

	//solve matrix
	local d is 1/((mA11*mA22) - (mA12*mA21)). // inverse coefficent
	//inverse matrix
	local dmA11 is d*mA22.
	local dmA12 is d*-1*mA12.
	local dmA21 is d*-1*mA21.
	local dmA22 is d*mA11.

	//Multiple inverse matrix by result matrix
	local A is dmA11*mC1 + dmA12*mC2.
	local B is dmA21*mC1 + dmA22*mC2.

	// //solve matrix
	// local d is ((mA11*mA22) - (mA12*mA21)). // inverse coefficent

	// //Multiple inverse matrix by result matrix
	// local A is (mA22*mC1 - mA12*mC2)/d.
	// local B is (mA11*mC2 - mA21*mC1)/d.



    return list(A, B).
}


function f_FlightAzimuth {
	parameter inc. // target inclination
    parameter V_orb. // target orbital speed
  
	// project desired orbit onto surface heading
	local az_orb is arcsin ( cos(inc) / cos(ship:latitude)).
	if (inc < 0) {
		set az_orb to 180 - az_orb.
	}
	
	// create desired orbit velocity vector
	local V_star is heading(az_orb, 0)*v(0, 0, V_orb).

	// find horizontal component of current orbital velocity vector
	local V_ship_h is ship:velocity:orbit - vdot(ship:velocity:orbit, up:vector)*up:vector.
	
	// calculate difference between desired orbital vector and current (this is the direction we go)
	local V_corr is V_star - V_ship_h.
	
	// project the velocity correction vector onto north and east directions
	local vel_n is vdot(V_corr, ship:north:vector).
	local vel_e is vdot(V_corr, heading(90,0):vector).
	
	// calculate compass heading
	local az_corr is arctan2(vel_e, vel_n).
	return az_corr.

}// End of Function

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function f_clearLine {
	parameter line.
	local i is 0.
	local s is "".
	until i = terminal:width {
		set s to " " + s.
		set i to i + 1.
	}
	print s at (0,line).
}
function f_f1_thrust {
	return max(0.001,((ship:AVAILABLETHRUST/(ship:mass*9.81))-1.20)/1.8).//3.0 to 1.20 TWR range
}

function f_Vel_Exhaust {
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	return g *isp.///thrust). //
}/// End Function

function f_mdot {
	local g is 9.80665.  // Gravitational acceleration constant used in game for Isp Calculation (m/s²)
	local engine_count is 0.
	local thrust is 0.
	local isp is 0. // Engine ISP (s)
	list engines in all_engines.
	for en in all_engines if en:ignition and not en:flameout {
	  set thrust to thrust + en:availablethrust.
	  set isp to isp + en:isp.
	  set engine_count to engine_count + 1.
	}
	set isp to isp / engine_count.
	set thrust to thrust* 1000.// Engine Thrust (kg * m/s²)
	return (thrust/(g * isp)). //kg of change
}/// End Function

function f_Tol {
//Calculates if within tolerance and returns true or false
	PARAMETER a. //current value
	PARAMETER b.  /// Setpoint
	PARAMETER tol.

	RETURN (a - tol < b) AND (a + tol > b).
}