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
        console.log(__charmRun(__charmPointer, input));
    }
}