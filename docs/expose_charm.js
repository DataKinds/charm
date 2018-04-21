// THIS FILE HAS TO BE INCLUDED _AFTER_ CHARM.JS HAS BEEN INCLUDED!

var __charmPointer = 0;
var __charmRun;

function initCharm() {
    __charmPointer = Module.ccall("initCapsule", "number", [], []);
    __charmRun = Module.cwrap("runCapsule", "string", ["number", "string"]);
}

function runCharm(input) {
    if (__charmPointer === 0) {
        alert("runCharm() called before initCharm() was called.");
    } else {
        var runOutput;
        try {
            runOutput = __charmRun(__charmPointer, input);
        } catch (error) {
            alert("Error running code `"+input+"`. This should never happen! Please report this to the devs along with what you were doing to cause this.");
        }
        console.log(runOutput);
        var o = document.getElementById("replOutput");
        o.textContent += "\n" + input + " --> " + runOutput;
    }
}