function nactiDatum() {
    fetch("http://localhost:8888/denvtydnu").then(function(response) {
        response.text().then(function(text) {
            let obj = JSON.parse(text);
            document.getElementById("den").innerHTML = obj.denVTydnuCesky;
            document.getElementById("datum").innerHTML = obj.datumCesky;
        });
    });
}