function updateHighscoreTable() {
    var url = "ajax/highscore_table.php";
    var xmlhttp;

    function onResponse() {
        var i;
        var hsTable;
        var entryElement;
        var speed;
        var correctness;

        if (xmlhttp.readyState != 4 || xmlhttp.status != 200) {
            return;
        }

        hsTable = "<h3>Najlepsze wyniki</h3>";
        hsTable += "<table class=\"highscore-table\">";
        entries =
            xmlhttp.responseXML.documentElement.getElementsByTagName("entry");
        hsTable += "<tr>";
        hsTable += "<th>Nazwa użytkownika</th>";
        hsTable += "<th>Prędkość<br />(znaki/min)</th>";
        hsTable += "<th>Prędkość<br />(słowa/min)</th>";
        hsTable += "<th>Poprawność</th>";
        hsTable += "</tr>";
        for (i = 0; i < entries.length; i++) {
            hsTable += "<tr>";
            entryElement = entries[i].getElementsByTagName("username");
            hsTable += "<td>" + entryElement[0].firstChild.nodeValue + "</td>";
            entryElement = entries[i].getElementsByTagName("speed");
            speed = entryElement[0].firstChild.nodeValue;
            hsTable += "<td><b>" + speed.replace('.', ',') + "</b></td>";
            hsTable += "<td>";
            hsTable += ((speed / 5).toFixed(1) + "").replace('.', ',');
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("correctness");
            correctness = entryElement[0].firstChild.nodeValue;
            hsTable += "<td>";
            hsTable += correctness.replace('.', ',') + "%";
            hsTable += "</td>";
            hsTable += "</tr>";
        }
        hsTable += "</table>";
        document.getElementById("highscoreTableArea").innerHTML = hsTable;
    }

    if (window.XMLHttpRequest) {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp = new XMLHttpRequest();
    } else {
        // code for IE5, IE6
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }
    xmlhttp.onreadystatechange = onResponse;
    xmlhttp.open("GET", url, true);
    xmlhttp.send(null);
}

updateHighscoreTable();
