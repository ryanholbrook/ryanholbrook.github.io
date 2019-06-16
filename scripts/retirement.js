function computePercent() {
    var startage = parseFloat(document.getElementById("startage").value)
    var retirementage = parseFloat(document.getElementById("retirementage").value)
    var yearsofsavings = parseFloat(document.getElementById("yearsofsavings").value)
    var percentofincome = parseFloat(document.getElementById("percentofincome").value) / 100
    var rateofreturn = parseFloat(document.getElementById("rateofreturn").value) / 100
    
    var N = retirementage - startage
    var p = 100 * yearsofsavings * percentofincome * rateofreturn / ((1 + rateofreturn)**N - 1)
    
    document.getElementById("percenttosave").innerHTML = Math.round(p) + "%"
};

document.addEventListener("DOMContentLoaded", function() {
    computePercent()
})
