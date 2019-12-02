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

Global t1 is 13.0.
Global t2 is 25.0.
Global t3 is 36.0.
Global t4 is 45.0.
Global t5 is 81.0.
Global t6 is 0.0.

Global tS1 is 35.0.
Global tS2 is 80.0.
Global tS3 is 115.0.
Global dtf is 0.

Global tAR is 160.0.//153 nominal but 160 for the tilt arrest on apollo 11

Wait 1. //Alow Variables to be set and Stabilise pre launch
PRINT "Prelaunch.".
Lock Throttle to f_f1_thrust().
Print f_f1_thrust().
//SET SHIP:CONTROL:PILOTMAINTHROTTLE TO 1.
//Wait for launch window //16/07/1969 at 13:32:00 GMT 18 197 13 22 00
Set vYear to 6.
Set vDay to 112.
Set vHour to 03.
Set vMinute to 59.
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

//Print Body:ATM:SEALEVELPRESSURE.
//Print Body:ATM:ALTITUDEPRESSURE(1230).
// Apollo 11 72.058 azimuth heading
Set sv_intAzimith to 72.058. 
Set basetime to time:seconds + 17.
Global INU_Zero is r(up:pitch,up:yaw,facing:roll) + r(0,0,sv_intAzimith-90).
Print INU_Zero.
LOCK STEERING TO INU_Zero. 
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
LOCK STEERING TO INU_Zero. //
wait until Time:HOUR >= vHour and Time:MINUTE >= vMinute and Time:SECOND >= vSecond.
Set liftoff to time:seconds.
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
Local dTTf is 1.
Local I is 1.
Local Majorloop is 0.
UNTIL time:seconds > (liftoff + tAR){
    Set tc to (time:seconds - liftoff).
	If (tc - Majorloop) > dTTf {
    	f_Pre_IGM(tc, sv_intAzimith).
		Set Majorloop to tc.
		//Print "Major loop:" + (tc - Majorloop).
	}
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
}//End Pre-IGM phase

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
//204.1 IGM phase 1 (Iterative guidance mode)
SET STEERINGMANAGER:MAXSTOPPINGTIME TO 1.0.
f_IGM_Steer(548, 191300, 191300, 31.381, 540, (5150000/(286900)), (1030000/(166300)), 4170, 248, 166300 ).

// 548.22 engine cutout and IGM phase 3
Wait UNTIL time:seconds > (liftoff + 548.22).
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
Wait UNTIL time:seconds > (liftoff + 699.34).
FOR eng IN engList { 
    IF eng:TAG ="APS" { 
        eng:shutdown.
    }
}
//555.6 tau mode

//561.0 ullage case jettison
//Wait UNTIL time:seconds > (liftoff + 561.0).
//Stage.
//562.4 end of tau mode
SET STEERINGMANAGER:MAXSTOPPINGTIME TO 0.1.
f_IGM_Steer_Orbit(665,191300,191300,31.381).

//665.2 begin terminal guidance

//691.6 end IGM phase 3

//699.34 ECO command
Wait UNTIL time:seconds > (liftoff + 699.34).
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
//699.8 APS ullage command
FOR eng IN engList { 
    IF eng:TAG ="APS" { 
        eng:activate.
    }
}
//709.34 Orbit instertion

//719.3 horitzontal move to
LOCK STEERING TO heading(90, 0).
//786.5 ullage engine turn off
Wait until ship:orbit:eccentricity < 0.01.
wait 2.
Print "Stage Finshed".
Shutdown. //ends the script
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
function f_Pre_IGM {
Parameter tc, Az.
    Local Xy is 0.
    Local Xz is 0.
    Local Xx is 0.
	//Print tc.

    If (tc < t1) or (alt:radar < 137){
			f_Yaw_Man(Tc).
			Return.//end function
	}

	If (tc <= t2){
		If (tc <= t6){
			//zero time locked at launch with engine out
		}
	}
	If (tc <= tAR){ f_Pre_IGM_Steer(tc).}
}

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

function f_IGM_Steer{
//Used for second stage taking into account the third "end" stage
/////////////////////////////////////////////////////////////////////////////////////
// Credits: Own modifications to:
// http://www.orbiterwiki.org/wiki/Powered_Explicit_Guidance
//With Large assisstance and corrections from:
// https://github.com/Noiredd/PEGAS
// https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19660006073.pdf
// https://amyparent.com/post/automating-rocket-launches/

Parameter End. //end looping time so go back to main function
Parameter tgt_pe. //target periapsis
Parameter tgt_ap. //target apoapsis
Parameter tgt_inc. //target inclination
Parameter Stage_End.//stage 1 time when fuel burnt out.
Parameter s_acc_end. // the estimate of the ships acceleration at the stage end (m/s)
parameter s2_acc_start. // the estimate of the ships acceleration at the next stage start (m/s)
parameter s_ve2. // the estimate of the ships exhaust velocity for next stage(m/s)
parameter mass_flow. // the estimate of the ships engine mass flowrate for next stage(kg/s)
parameter start_mass. // the estimate of next stage starting mass(kg)
parameter u is 0. // target true anomaly in degrees(0 = insertion at Pe)
parameter tau2 is 665.
parameter T2 is 99.8.

    Local tc to (time:seconds - liftoff). // current mission time progression since lift off
    local last is tc. //starting mission elased time at last point of calculation
    local A1 is -0.3. //starting peg variable
    local B1 is 0. //starting peg variable
    local C1 is 0.1. //starting peg variable

    local A2 is 0. //starting peg variable
    local B2 is 0. //starting peg variable
    local C2 is 0.1. //starting peg variable

    local converged is -2. // used by convergence checker
    local delta is 0. //time between peg loops
	local T1 is Stage_end - Tc. //the time to thrust cut off as we know this stage will be fully used up.
	local peg_step is 1.0.//time between each calcuation check
    local s_r is ship:orbit:body:distance. // current ship radius
    local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship acceleration

    local s_vy is ship:verticalspeed. // current ship veritcal speed
    local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2). // current ship horizontal speed
	local s_ve is f_Vel_Exhaust(). // current ship exhaust velocity
	local w is sqrt((s_vx^2) + (s_vy^2)) / (s_r). //the angular velocity (radians per second) which is the tangential or the combined x and y components of velocity 
	Set tau to s_ve/s_acc. //time to burn ship if all propellant
	
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
	Set T1 to Stage_end - Tc.

	Clearscreen.
	Print "IGM Phase 1" .//AT (0,1).
    Print "Action: IGM Convergence loop" .//AT (0,2).
	//Loop through updating the parameters until the break condition is met
    until false {
		// collect current ship parameter
        Set tc to (time:seconds - liftoff).
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		Set w to s_vx / s_r.
		If tau_lock = false{
			Set tau to s_ve/s_acc.
		} else{
			Set tau to T1*(1243.77/1009.04).
		}
        set delta to tc - last. // time since last calculation

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
			f_clearLine(2).
			Print "Action: CECO" .//AT (0,2).
			Print "Speed: " + SHIP:AIRSPEED .//AT (0,3).
			Print "Altitude: " + altitude .//AT (0,4).
			Print "Mass: " +ship:mass .//AT (0,5).
			Print "Dry Mass: " +ship:drymass .//AT (0,6).
			Set I to I+1.
		}

		// 494.4 IGM phase 2 and tau mode
		if (time:seconds > (liftoff + 494.4)) and (I = 1){
			f_clearLine(1).
			Print "IGM Phase 2 and Tau mode" AT (0,1).
			Set tau_lock to true.
			Set I to I+1.
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
			f_clearLine(2).
			Print "Action: CECO" .//AT (0,2).
			Print "Speed: " + SHIP:AIRSPEED .//AT (0,3).
			Print "Altitude: " + altitude .//AT (0,4).
			Print "Mass: " +ship:mass .//AT (0,5).
			Print "Dry Mass: " +ship:drymass .//AT (0,6).
			Set I to I+1.
		}
		// 504.2 end tau mode
		if (time:seconds > (liftoff + 494.4)) and (I = 3){
			f_clearLine(1).
			Print "IGM Phase 2" AT (0,1).
			Set tau_lock to false.
			Set I to I+1.
		}

		// 548.22 engine cutout and IGM phase 3
		if (time:seconds > (liftoff + 547.5)) and (I = 4){
			LOCK STEERING TO heading(90, s_pitch).
			Break. //back up break incase the bottom one doesn't work.
		}

        If (delta >= peg_step) {  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle calculations
			Print "######Convergence Step######" AT (0,9).
			Set last to (time:seconds - liftoff).//reset last
			Set T1 to Stage_end - Tc. //
			Set A1 to A1 + (B1 * (Stage_end - last)).

			/// determine vertical state based on A1 and B1
			local peg_w is f_PEG_ABT_2(A1, B1, Stage_end, s_acc_end, w_T1, tgt_vy, tgt_r, tgt_h, tau2, T2, s_ve2, start_mass, mass_flow, s2_acc_start, tgt_w).

			set A1 to peg_w[0].
            set B1 to peg_w[1].

			set T2 to peg_w[2].
			set A2 to peg_w[3].
			Set B2 to peg_w[4].
			Set C2 to peg_w[5].

			Set rT1 to peg_w[6].
			set w_T1_new to peg_w[7].

			set CT to peg_w[8].
			set dv2 to peg_w[9].

			set dA to peg_w[10].
			set dB to peg_w[11].

			set C to peg_w[12].
			set tau2 to peg_w[13].

			local tgt_vy1 is 73.66.
			local tgt_vx1 is 6918.6.
			local tgt_r1 is 6559612.
			

			Print "Conv Step percent: " + f_Tol (w_T1, w_T1_new, abs(w_T1*0.001)) .//AT (0,11).
			Print w_T1.
			Print w_T1_new.

			if f_Tol (w_T1, w_T1_new, abs(w_T1*0.001)) { // check if old w_T1 and new w_T1 are converged
                if converged < 0 {
                    set converged to converged+1. //(this is done over 3 cycles to ensure the convergence solution selected is accurate enough over ten ship location updates rather than relying on only one convergence solution to enter a closed loop)
			 		f_clearLine(10).
			 		Print "Convergence step +1" .//AT (0,10).
                 } else if converged = 0 {
                    set converged to 1.
			 		f_clearLine(10).
                    Print("closed loop enabled") .//AT (0,10).
        	    }
            } 
			set w_T1 to w_T1_new.

			Set delta to 0.
			
			set s_pitch to A1 + C. //sin pitch at current time.
			set s_pitch to max(-0.707, min(s_pitch, 0.707)). // limit the pitch change to between 1 and -1 with is -90 and 90 degress
			set s_pitch to arcsin(s_pitch). //covert into degress check
			Print "S pitch: " + s_pitch.
        } 

        if converged = 1 {
			LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
			
            if (T1 <0.2){
				LOCK STEERING TO heading(90, s_pitch).
                break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
            }
        }
		
		Print "Pitch: " + s_pitch .//AT (0,13).
		Print "T1: " + T1 .//AT (0,14).
		Print "A1: " + A1 .//AT (0,15).
		Print "B1: " + B1 .//AT (0,16).

		Print "tau: " + tau .//AT (0,18).
		Print "C: " + C .//AT (0,19).


		Print "Staging w: " + w_T1 .//AT (0,21).
		Print "Staging height: " + (rT1 - body:radius) .//AT (0,22).
		Print "Staging vo: " + w_T1 * rT1 .//AT (0,23).
		Print "Staging Pitch : " + arcsin( max(-0.9, min(A1+C1,0.9)) ) .//AT (0,24).

		Print "T2: " +T2 .//AT (0,25).
		Print "A2: " +A2 .//AT (0,26).
		Print "B2: " +B2 .//AT (0,27).
		Print "C2: " +C2 .//AT (0,28).

		Print "delta: " + delta .//AT (0,30).
		Print "missiontime: " + missiontime .//AT (0,31).

		Print "CT: " + CT .//AT (0,33).
		Print "dv2: " + dV2 .//AT (0,34).
		Print "tau2: " + tau2.//AT (0,35).
		Print "dA: " + dA .//AT (0,36).
		Print "dB: " + dB .//AT (0,37).

        wait 0.1. 
		if(End-tc < 1){
			break. 
		}
    }

} // end of function
	

function f_PEG_ABT_2 {
    parameter A1.
    parameter B1.
    parameter Stage_end.
	parameter s_acc_end.
	parameter w_T1. /// this is what we are trying to determine
	parameter tgt_vy. // orbit "target" vertical velocity
	parameter tgt_r. // orbit "target" radius
	parameter tgt_h. // orbit "target" angular momentum
	parameter tau2.
	parameter T2.
	parameter s_ve2.
	parameter start_mass.
	parameter mass_flow.
	parameter s2_acc_start.
	parameter tgt_w.

	Local T1 is 0.
	Local A2 is 0.
	Local B2 is 0.
	Local C2_T1 is 0. 
	Local rT1 is 0. 
	Local C2_T is 0. 
	Local dv2 is 0. 
	Local dA is 0. 
	Local dB is 0. 
	Local C is 0.
	Local hT1 is 0. 
	Local frT1 is 0. 
	local w_T1_new is 0. 
	local dW is 0.

	local s_vy is ship:verticalspeed.
	local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
	local s_r is ship:orbit:body:distance.
	local s_ve is f_Vel_Exhaust().
	local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	local tau is s_ve/s_acc. //time to burn ship if all propellant
	
	local h0 is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. //current angular momentum

	Set Tc to (time:seconds - liftoff).
	Set T1 to Stage_end - Tc.

	//stage intergral calculations
		local X4 is G * Xs.
		local L1 is s_ve*(LN(tau/(tau - T1))).
		local J1 is (L1 * tau) - (s_ve*T1).
		local S1 is (L1*T1) - J1.
		local Q1 is (S1*tau) - ((s_ve*(T1^2))/2).
		local P1 is (J1*T1)-((s_ve*(T1^2))/2).
		local U1 is (Q1*tau) - ((s_ve*(T1^3))/6).

		local L2 is s_ve2*(LN(tau2/(tau2 - T2))).
		local J2 is (L2 * tau2) - (s_ve2*T2).
		local S2 is (L2*T2) - J2.
		local Q2 is (S2*tau2) - ((s_ve2*(T2^2))/2).
		local P2 is (J2*T2)-((s_ve2*(T2^2))/2).
		local U2 is (Q2*ta2) - ((s_ve2*(T2^3))/6).

		Local L12 is L1 + L2.
		Local J12 is J1+J2+(L2*T1).
		Local S12 is S1 - J2 + (L12*(T2 + Tc)).
		Local Q12 is Q1+Q2+(S2*T1)+(J1*T2).
		Local P12 is P1+P2+(T1*((2*J2)+(L2*T1))).
		Local U12 is U1+U2+(T1*((2*Q2)+(S2*T1))) +(T2*P1).

		local L3 is s_ve3*(LN(tau3/(tau3 - T3))).
		local J3 is (L3 * tau3) - (s_ve3*T3).
		Local Ly is L12 +L3.
		Local T1c is T1+T2+Tc.
		Local Tt is T1c + T3.

		local loop is -1.

		until loop > 0{
			//range angle 1
			local vel is ship:velocity:orbit:mag.

			delta2 is (vel * Tt) - J3+(Ly*T3) - (ROV/s_ve3)*((tau - T1)*L1) + ((tau2-T2)*L2) + ((tau3-T3)*L3)*(Ly+vel-Vt).
			Phi_T is arctan (Z4/X4) + (1/Rt) * (S12+delta2)*cos(Gamma_T).

			// // no option 2, no non-freeze terminal values, rotate values for into orbit, no update : those are for out of orbit.
			
			// //range angle 2
			// //Rt,Vt and Gamma_T computations

			// local f is Phi_T + aD.
			// local RT is p/(1+ (e*cos(f))).
			// local Vt is K5*sqrt(1 + 2*e cos(f) + e^2).
			// local gamma_T = arctan((e*sin(f))/(1+e*cos(f))).
			// local GT = -ship:body:mu / (RT^2).
			// // rotate terminal conditions

			// //update yes

			local epsT is RT.
			local sigma_dotT is VT*cos(gamma_T).
			local sigma_DdotT is VT*sin(gamma_T).
			local esp_dotT is 0.
			local eps_DdotT is GT.

			///estimated time to go

			local d_esp_dot is epsT - eps_DdotT - (eps_Ddot*Tt).
			local d_eta_dot is - eta_dot - (eta_Ddot*Tt).
			local d_sigma_dot is sigma_dotT - sigma_dot - (sigma_Ddot*Tt).
			local d_L3 is sqrt(((((d_esp_dot)^2) + ((d_eta_dot)^2) + ((d_sigma_dot)^2)) / Ly ) - Ly).
			local d_T3 is (d_L3*(tau3-T3))/s_ve3.
			local T3 is T3 + d_T3.
			local Tt is Tt + d_T3.
			local L3 is L3 + d_L3.
			Local J3 is J3 + (d_L3*T3).
			set loop to loop +1.

		}

		//xy and xp calculations
		local S3 is (L3*T3) - J3.
		local Q3 is (S3*tau3) - ((s_ve3*T3)/2).
		local P3 is (J3*(tau3 + 2*T1c)) - (s_ve3*(T3^2)/2).
		local U3 is (Q3*(tau3 + 2*T1c)) - (s_ve3*(T3^3)/6).

		local d_esp_dot is d_esp_dot - (eps_DdotT*d_T3).
		local d_eta_dot is d_eta_dot - (eta_Ddot*d_T3).
		local d_sigma_dot is d_sigma_dot - (sigma_Ddot*d_T3).
		local Ly is L12 + L3.
		local Xz is arcsin (d_eta_dot/Ly).
		local Xy is arctan (d_esp_dot/d_sigma_dot).

		//if Tt < 10 then the time ti begin chi tilde mode is yes and altitude constraint is removed.

		//Yaw steering parameters
		local Jy is J12 J3 + (L3 T1c).
		local Sy is S12 - J3 + (Ly*T3).
		local Qy is Q12 + Q3 +(S3*T1c) + ((Tc + T3)*J12).
		local Ky is Ly/Jy.
		local Dy is Sy - (Ky*Qy).
		local d_eta - eta + (eta_dot*Tt) + ((eta_Ddot*(Tt^2))/2) + (Sy*sin(Xz)).
		local K3 is K3*(1-(K3^2)/TRY).
		local K4 is Ky*K3.

		//pitch steering parameters
		local Lp is Ly*cos(Xz).
		local C2 is cos(Xz) + (K3*sin(Xz)).
		local C4 is K4*sin(Xz).
		local Jp is (Jy*C2) - C4*(P12 +P3 + (T1c^2)*L3).
		local Sp is Sy*C2 - C4*Qy.
		local Qp is Qy*C2 - C4*(U12 + U3 + (T1c^2)*S3 + P12*(T3+Tc) ).
		local Kp is Lp/Jp.
		local Dp is Sp-Kp*Qp.
		local d_sigma is sigma - sigma_T + ((sigma_Ddot*(Tt^2))/2) + (Sp*sin(Xy)).
		local Kt is d_sigma/Dp*cos(Xy).
		local K1 is K1*(1-(K1^2)/TRP).
		local K2 is Kp*K1.

		// guidance time update

		//Steering angles
		local Xzdd is Xz - K3+(K4*t)
		local Xydd is Xy - K1+(K2*t)
		

		local rdotT1 to s_vy +bb01*A1 + bb11*B1. //vertical speed at staging
		set rT1 to s_r + (s_vy*T1) + (cc01 * A1) + (cc11*B1). // vertical radius at staging

		Print "Checks check:".
		Print "s_r: "+s_r.
		Print "tgt_r: "+tgt_r.
		Print "A1: " + A1.
		Print "B1: " + B1.
		Print "rdotT1: "+rdotT1.
		Print "rT1: "+rT1.
		Print "Height: "+ (rT1 - body:radius).

		If rT1 > tgt_r{
			set rT1 to tgt_r.
		}

		If rT1 < s_r{
			set rT1 to s_r.
		}

	//Current pitch guidance for horizontal state
		Set C to ((body:mu/(s_r^2)) - ((w^2)*s_r))/s_acc. //portion of vehicle acceleration used to counteract gravity
		local fr is A1 + C. //sin pitch at current time
		local count is 0.
	Until false{	
	// Determine horizontal state
		If count > 10{
			break.
		}
		Set count to count +1.//prevents stuck in loop
		local C_T1 is (body:mu/(rT1^2)) - ((w_T1^2)*rT1). //Gravity and centrifugal force term at cutoff
		Set C_T1 to C_T1 /s_acc_end. 
		Set frT1 to A1 + (B1*T1) + C_T1. //sin pitch at burnout. 
		local frdot is (frT1-fr)/T1. //approximate rate of sin pitch
		local ft is 1 - (frT1^2)/2. //cos pitch
		local ftdot is -fr*frdot. //cos pitch speed
		local ftdd is -(frdot^2)/2. //cos pitch acceleration
		
		local dhT1 to ((s_r + rT1)/2)*( (ft*bb01) + (ftdot*bb11) + (ftdd*bb21) ). //angular momentum a change to staging.
		Set hT1 to dhT1 + h0.

		local v0_T1 is hT1/rT1.
		Set v0_T1 to 6919.
		Set rT1 to 6559612.
		//Real rT1 is 6559612 or 187319m at 0.62 degrees
		//Real V0_T1 is 6513 or 6919 Surface reference
		
		//local v0_T1 is 7007.//this is the actual value given in apollo program
		//Set rT1 to v0_T1/hT1.

		Set w_T1_new to v0_T1/rT1.
		
		//If w_T1 > tgt_w{
			//set w_T1 to tgt_w.
		//}
		Print "Converg test " +f_Tol (w_T1, w_T1_new, abs(w_T1*0.01)).
		Print w_T1.
		Print w_T1_new.
		if f_Tol (w_T1, w_T1_new, abs(w_T1*0.01)) { // check if old w_T1 and new w_T1 are converged
			break.
		}else{
			set dW to abs (w_T1_new - w_T1).
			set w_T1 to w_T1_new.
		}


		//Print "Horizontal check:".
		print "hT1: "+v0_T1.
		print "v0_T1: "+v0_T1.
		Print "rT1: "+rT1.
		print "w_T1: "+w_T1.
		Print "dW: "+dW.
		Print "test over".
		wait 0.1.
	}
	//Guidance staging discontinuities

		Local dA is ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ).
		Set dA to dA * ( (1/s_acc_end) - (1/s2_acc_start) ).

		Local dB is - ( (body:mu/(rT1^2)) - ((w_T1^2)*rT1) ) * ( (1/s_ve) - (1/s_ve2)  ). 
		Set dB to dB + ( ( (3*(w_T1^2)) - ((2*body:mu)/(rT1^3)) ) *rdotT1* ( (1/s_acc_end) - (1/s2_acc_start) )  ).

	//Solve Explicit Guidance Equations for A2 and B2 to compare w_T1 to w at T2 start

		local F2 is start_mass*s2_acc_start. // starting thrust
		local a2_T2 is F2 / (start_mass - (mass_flow*T2)). // acceleration at T2

		set tau2 to s_ve2/s2_acc_start.
		//Set tau2 to 665.86. //Apollo saturn V fixed this value

		local dh is tgt_h - hT1. //angular momentum to gain in next stage
		local mean_r is (tgt_r + rT1)/2.

		local dv_gain is dh/mean_r.
		Print "dv gain T1 to T2: " + dv_gain.
		local T_plus is tau2*(1 - constant:e ^ (-dv_gain/s_ve2)).
		Print "T2 plus: " + T_plus.
		Set T2 to T_plus.
		
		// //rate at second stage start
		Set C2_T1 to (body:mu/(rT1^2)) - ((w_T1^2)*rT1).
		Set C2_T1 to C2_T1/s2_acc_start. 
		local frT1 is A2 + C2_T1. 

		// calculate burnout variables
		set C2_T to (body:mu/(tgt_r^2)) - ( (tgt_w^2)*tgt_r ).
		Set C2_T to C2_T/a2_T2.

		local fr_T is A2 + (B2*T2) + C2_T. //sin pitch at burnout.
		local frdot_T is (fr_T-frT1)/T2. //approximate rate of sin pitch at burnout 

		local ft_T is 1 - (fr_T^2)/2. //burnout cos pitch
		local ftdot_T is -fr_T*frdot_T. //staging cos pitch speed
		local ftdd_T is -(frdot_T^2)/2. //staging cos pitch acceleration

		set dv2 to (dh/mean_r) + ((s_ve2*T2) * (ftdot_T+(ftdd_T*tau2))) + ((ftdd_T*s_ve2*(T2^2))/2). 
		set dv2 to dv2 / (ft_T + (ftdot_T*tau2) + (ftdd_T*(tau2^2))). // ideal velocity to gain in second stage

		local T_plus is tau2*(1 - constant:e ^ (-dv2/s_ve2)). // estimated updated burnout time

		Print "T2: " +T2.
		Print "T2 plus 2: " + T_plus.
		//Set T_plus to 120.5.//this is fixed value in the Saturn V apollo
		Set T2 to T_plus.

		
		//Functuions For second stage

		Local bb02 is -s_ve2*(LN(1-(T2/tau2))).
		Local bb12 is (bb02 * tau2) - (s_ve2*T2).
		Local bb22 is (bb12 * tau2) - ((s_ve2*(T2^2))/2).
		Local cc02 is (bb02*T2)-bb12.
		Local cc12 is (cc02*tau2) - ((s_ve2*(T2^2))/2).

		//set up matricies
		local mA11 is bb01 + bb02.
		local mA12 is bb11 + bb12 + (bb02*T1).
		local mA21 is cc01 + cc02 + (bb02*T2). 
		local mA22 is cc11 + (bb11*T2) + (cc02*T1) + cc12.

    	local mC1 is tgt_vy - s_vy - (bb02*dA) - (bb12*dB). 
    	local mC2 is tgt_r - s_r - (s_vy*(T1 +T2)) - (cc02*dA) - (cc12*dB). 

		local peg is f_peg_solve(mA11, mA12, mA21, mA22, mC1, mC2).

		Set A1 to peg[0].
		Set B1 to peg[1].
		Print "A1"+ A1.
		Print "B1" + B1.
		wait 0.1.

		local v0_T2_0 is (tgt_h/tgt_r) - dv2.
		Local w_T2_0 is v0_T2_0/rT1.

		Print "****check 2*****".
		Print "rT1: "+rT1.
		Print "w_T2_0: "+w_T2_0.
		Print "w_T1: " +w_T1.
		Print "%: " + ((1-(w_T2_0/w_T1))*100).

		/// Determine A2 and B2

		set A2 to dA + A1 + (B1*T1).
		set B2 to dB + B1.
		
		//if f_Tol (w_T1, w_T2_0, abs(w_T1*0.005)){
			//Break.
		//}
		wait 0.1.
	//}
	Return list(A1, B1, T2, A2, B2, C2_T1, rT1, w_T1, C2_T, dv2, dA, dB, C, tau2). 
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

function f_IGM_Steer_orbit{

/////////////////////////////////////////////////////////////////////////////////////
//Used on the final stage only

parameter End.
Parameter tgt_pe. //target periapsis
Parameter tgt_ap. //target apoapsis
Parameter tgt_inc. //target inclination
Parameter u is 0. // target true anomaly in degrees(0 = insertion at Pe)

//values setup
    set ra to body:radius + tgt_ap. //full Ap
    set rp to body:radius + tgt_pe. //full pe
    local sma is (ra+rp)/2. //sma
    local ecc is (ra-rp)/(ra+rp). //eccentricity
    local vp is sqrt((2*body:mu*ra)/(rp*2*sma)). // this is the target velocity at the periapsis
	Print "vp " +vp. 
    local rc is (sma*(1-ecc^2))/(1+ecc*cos(u)). // this is the target radius based on the desire true anomoly
    print "rc "+rc.
    local vc is sqrt((vp^2) + 2*body:mu*((1/rc)-(1/rp))). // this is the target velocity at the target radius (if u is zero this will equal vp)
    print "vc "+vc.
    local uc is 90 - arcsin((rp*vp)/(rc*vc)). // this is the direction vector of the target velocity
    // Define target position and velocities
    local tgt_r is rc.
    local tgt_vy is vc*sin(uc). // this is the split of the target velocity at the point in time
    local tgt_vx is vc*cos(uc). // this is the split of the target velocity at the point in time (should be zero for u = 0)
	Print "tgt_vx " + tgt_vx.
	Print "tgt_vy " + tgt_vy.
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag. // target angular momentum
	wait 0.1.
    Local tc to (time:seconds - liftoff). // current mission time progression since lift off
    local last is tc. //starting mission elased time at last point of calculation
    local A is 0. //starting peg variable
    local B is 0. //starting peg variable
    local C is 0. //starting peg variable
    local converged is -3.
    local delta is 0. //time between peg loops
	local T is 50. //intial guess on time to thrust cut off

	local peg_step is 1.0.//time between each calcuation check
    
    local s_r is ship:orbit:body:distance. // current ship parameter
    local s_acc is ship:AVAILABLETHRUST/ship:mass. // current ship parameter
	Print "s_acc " + s_acc.
    local s_vy is ship:verticalspeed. // current ship parameter
    local s_vx is sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2). // current ship parameter
	local s_ve is f_Vel_Exhaust().
	Print "Vel exhaust: " + s_ve.
	local w is s_vx / s_r.
	Set tau to s_ve/s_acc. //time to burn ship if all propellant
	Print "tau: " + tau.
    local peg is f_PEG_ABT(A, B, T, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc).  // inital run through the cycle with first estimations
    wait 0.001.

    Print "Entering Convergence loop".
	//Loop through updating the parameters until the break condition is met
    until false {
		// collect current ship parameter
        Set tc to (time:seconds - liftoff).
        set s_r to ship:orbit:body:distance.
		set s_acc to ship:AVAILABLETHRUST/ship:mass.
		set s_vy to ship:verticalspeed.
		set s_vx to sqrt(ship:velocity:orbit:sqrmagnitude - ship:verticalspeed^2).
		Set tau to s_ve/s_acc.
		Set w to s_vx / s_r.
        set delta to tc - last. // time since last calculation
		Print "Mission Time " + tc.
		Print "delta " + delta.
		Print "peg_step " + peg_step.
		
        If (delta >= peg_step) {  // this is used to ensure a minimum time step occurs before undertaking the next peg cycle
            //f_peg_vis().
			Print "Convergence Step#####################################################".
			Set peg to f_PEG_ABT (A, B, T, peg_step, tau, tgt_vy, tgt_vx, tgt_r, s_vy, s_vx, s_r, s_acc, 8).
			Set last to (time:seconds - liftoff).
			Print "Con Step percent: " + abs( (T-2*peg_step)/peg[3]-1 ).
			Print T.
			Print peg[3].
			Print delta.
			if abs( (T-2*peg_step)/peg[3]-1 ) < 0.02 {  //if the time returned is within 2% of the old T guess to burnout allow convergence to progress 
				//ClearScreen.
                if converged < 0 {
                    set converged to converged+1. //(this is done over 3 cycles to ensure the convergence solution selected is accurate enough over ten ship location updates rather than relying on only one convergence solution to enter a closed loop)
					Print "Convergence step +1".
                } else if converged = 0 {
                    set converged to 1.
                    Print("closed loop enabled").
                }
            } 

            set A to peg[0].
            set B to peg[1].
            set C to peg[2].
            set T to peg[3].
			Set delta to 0.
            
        } 
 		set s_pitch to (body:mu/(s_r^2)) - ((w^2)*s_r).
		set s_pitch to A + (s_pitch / s_acc).
		set s_pitch to max(-0.1, min(s_pitch, 0.1)). // limit the pitch change to between 1 and -1 with is -90 and 90 degress
        Set s_pitch to arcsin(s_pitch). //covert into degress check
		Print "s_pitch final " + s_pitch.

        if converged = 1 {
			//LOCK STEERING TO heading(f_FlightAzimuth(tgt_inc, tgt_vx), s_pitch).
			LOCK STEERING TO heading(90, s_pitch).
			ClearScreen.
			Print "closed loop Steering".
			Print "Pitch: " + s_pitch.
			Print (A + B*T + C).
			Print "A: " + A.
			Print "B: " + B.
			Print "C: " + C.
			Print "T: " + T.
			Print "tau: " + tau.
			Print "CT:" + peg[4].
			Print "dv:" + peg[5].
			Print "delta: " + delta.
			Print "missiontime: " + missiontime.
			Print abs(T - delta).
			
            if(End-tc < 1) or (T <0.2){
                break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
            }
        }
        wait 0.1. 
		if(End-tc < 1){
			break. //break when the time left to burn minus the last step incriment is less than 0.2 seconds remaining so we do not enter that last few step(s) where decimal and estmation accuracy becomes vital.
		}
    }

} // end of function


///////////////////////////////////////////////////////////////////////////////////
function f_PEG_ABT {
    parameter oldA.
    parameter oldB.
    parameter oldT.
    parameter delta.
	parameter tau.
	parameter tgt_vy.
	parameter tgt_vx.
	parameter tgt_r.
	parameter s_vy.
	parameter s_vx.
	parameter s_r.
	parameter s_acc.
	parameter HSL is 0.
    
	local A is 0.
    local B is 0.
    local C is 0.
    local T is 1.

	local s_ve is f_Vel_Exhaust().
    Local tgt_h is vcrs(v(tgt_r, 0, 0), v(tgt_vy, tgt_vx, 0)):mag.
	///if first time through get inital A and B values
    if oldA = 0 and oldB = 0 {
        local ab is f_peg_solve(oldT, tau, tgt_vy, tgt_r, s_vy, s_r, s_ve).
        set oldA to ab[0].
        set oldB to ab[1].
    }
	local T_dash is oldT - delta.
    local h0 is vcrs(v(s_r, 0, 0), v(s_vy, s_vx, 0)):mag. //current angular momentum
    local dh is tgt_h - h0. //angular momentum to gain
	
    Local C is (body:mu/s_r^2 - (((s_vx^2)+(s_vy^2))/s_r))/s_acc. //current portion of vehicle acceleration used to counteract gravity
	local CT is ((body:mu/tgt_r^2) - (((tgt_vx^2)+(tgt_vy^2))/tgt_r)) / (s_acc / (1-(oldT/tau))). //Gravity and centrifugal force term at cutoff

    local fr is oldA + C. //sin pitch at current time	
	local frT is oldA + oldB*oldT + CT. //sin pitch at burnout.
	local frdot is (frT-fr)/oldT. //approximate rate of sin pitch

    local ft is 1 - (fr^2)/2. //cos pitch
    local ftdot is -fr*frdot. //cos pitch speed
    local ftdd is -(frdot^2)/2. //cos pitch acceleration

    local mean_r is (tgt_r + s_r)/2.
    local dv is (dh/mean_r) + ((s_ve*T_dash) * (ftdot+(ftdd*tau))) + ((ftdd*s_ve*(T_dash^2))/2). // ideal velocity to gain. note this is from nasa manual equation 36 and wiki
	//Print (ft + ftdot*tau + ftdd*(tau^2)).
    set dv to dv / (ft + (ftdot*tau) + (ftdd*(tau^2))). // big equation from wiki near end of estimated
	//Print "DV: " + dv.
    local T_plus is tau*(1 - constant:e ^ (-dv/s_ve)). // estimated updated burnout time

    local b0 is -s_ve * LN(1 - (T/tau)). //Wiki eq 7a
    local b1 is (b0*tau) - (s_ve*T). //Wiki eq 7b
    local c0 is b0*T - b1. //Wiki eq 7c
    local c1 is (c0*tau) - (s_ve * T^2)/2. //Wiki eq 7d
	
    local mb0 is tgt_vy - s_vy.  //Wiki Major loop algortthm MB Matrix top
    local mb1 is (tgt_r - s_r) - s_vy*T. //Wiki Major loop algortthm MB Matrix bottom

	//set up matricies
	local mA11 is b0.
	local mA12 is b1.
	local mA21 is c0. 
	local mA22 is c1.

    local mC1 is mb0. 
    local mC2 is mb1. 

    if(T1 >= HSL) { // HSL check time to go is above (8 seconds is apollo 11 e3 value see para 4.1.7), below this the solution starts to become very sensitive and A and B should not longer be re-calculated
        local ab is f_peg_solve(mA11, mA12, mA21, mA22, mC1, mC2).
        set A to ab[0].
        set B to ab[1].
   } else {
        Print ("Terminal guidance enabled"). // keep A and B constants static
        set A to oldA.
        set B to oldB.
   }
	wait 0.01.
    return list(A, B, C, T_plus, CT, dv).
}
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

function f_Tol {
//Calculates if within tolerance and returns true or false
	PARAMETER a. //current value
	PARAMETER b.  /// Setpoint
	PARAMETER tol.

	RETURN (a - tol < b) AND (a + tol > b).
}