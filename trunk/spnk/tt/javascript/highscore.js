function updateHighscoreTable(page) {
    var url = "tt/ajax/highscore_table.php";
    var request;
    var PAGE_SIZE = 20;
    var currentPageElement;

    function onResponse() {
        var USERNAME_CUT = 19;
        var usernameCut;
        var i;
        var hsTable;
        var entries;
        var entryElement;
        var speed;
        var correctness;
        var username;
        var totalHSSize;

        function getHighscorePaging(currentPage, pageCount) {
            var i;
            var hsPaging = "";

            if (currentPage === 1) {
                hsPaging += "&larr; ";
            } else {
                hsPaging += "<a href=\"#\" onClick=" +
                        "\"updateHighscoreTable(" + (currentPage - 1) + "); " +
                        "return false;\">" +
                        "&larr; </a>";
            }
            for (i = 1; i <= pageCount; i++) {
                if (i === currentPage) {
                    hsPaging += "<span id=\"currentPage\">" + i + "</span> ";
                } else {
                    hsPaging += "<a href=\"#\" onClick=" +
                            "\"updateHighscoreTable(" + i + "); " +
                            "return false;\">" +
                            i + "</a> ";
                }
            }
            if (currentPage === pageCount) {
                hsPaging += "&rarr; ";
            } else {
                hsPaging += "<a href=\"#\" onClick=" +
                        "\"updateHighscoreTable(" + (currentPage + 1) + "); " +
                        "return false;\">" +
                        "&rarr; </a>";
            }
            return hsPaging;
        }

        function trim(str) {
            return str.replace(/^\s+|\s+$/g, "");
        }

        if (request.readyState != 4 || request.status != 200) {
            return;
        }
        hsTable = "<h3>Najlepsze wyniki</h3>";
        hsTable += "<table class=\"highscore-table\">";
        entries =
            request.responseXML.documentElement.getElementsByTagName("entry");
        hsTable += "<tr>";
        hsTable += "<th>Miejsce</th>";
        hsTable += "<th style=\"padding-right: 20;\">Nazwa u¿ytkownika</th>";
        hsTable += "<th>Prêdko¶æ<br>(znaki/min)</th>";
        hsTable += "<th>Prêdko¶æ<br>(s³owa/min)</th>";
        hsTable += "<th>Poprawno¶æ</th>";
        hsTable += "</tr>";
        for (i = 0; i < entries.length; i++) {
            if (i % 2 === 0) {
                hsTable += "<tr style=\"background: #F1F1DD;\">";
            } else {
                hsTable += "<tr>";
            }
            entryElement = entries[i].getElementsByTagName("rank")[0];
            hsTable += "<td>" + entryElement.firstChild.nodeValue + "</td>";
            entryElement = entries[i].getElementsByTagName("username")[0];
            username = entryElement.firstChild.nodeValue;
            usernameCut = USERNAME_CUT;
            if (username.toUpperCase() === username) {
                usernameCut -= 4;
            }
            hsTable += "<td style=\"text-align: left; padding-right: 0;\"";
            if (username.length > usernameCut) {
                hsTable += " title=\"" + username + "\"";
                username = trim(username.substring(
                        0, usernameCut - 1)) + "...";
            }
            hsTable += ">";
            hsTable += username;
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("speed")[0];
            speed = entryElement.firstChild.nodeValue;
            hsTable += "<td><b>" + speed.replace('.', ',') + "</b></td>";
            hsTable += "<td>";
            hsTable += ((speed / 5).toFixed(1) + "").replace('.', ',');
            hsTable += "</td>";
            entryElement = entries[i].getElementsByTagName("correctness")[0];
            correctness = entryElement.firstChild.nodeValue;
            hsTable += "<td>";
            hsTable += correctness.replace('.', ',') + "%";
            hsTable += "</td>";
            hsTable += "</tr>";
        }
        for (i = entries.length; i < PAGE_SIZE; i++) {
            hsTable += "<tr><td colspan=\"5\">&nbsp;</td></tr>";
        }
        hsTable += "<tr><td colspan=\"5\" style=\"text-align: center; " +
                "vertical-align: bottom; height: 32px;\">";
        totalHSSize =
                request.responseXML.documentElement.getElementsByTagName(
                    "totalSize")[0].firstChild.nodeValue;
        if (totalHSSize > PAGE_SIZE) {
            hsTable += getHighscorePaging(
                    page, parseInt(((totalHSSize - 1) / PAGE_SIZE) + 1, 10));
        }
        hsTable += "</td></tr>";
        hsTable += "</table>";
        document.getElementById("highscoreTableArea").innerHTML = hsTable;
    }

    if (page === undefined) {
        currentPageElement = document.getElementById("currentPage");
        if (currentPageElement) {
            page = Number(currentPageElement.innerHTML);
        } else {
            page = 1;
        }
    }
    try {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        request = new XMLHttpRequest();
    } catch (e) {
        // code for IE5, IE6
        request = new ActiveXObject("Microsoft.XMLHTTP");
    }
    request.onreadystatechange = onResponse;
    url += "?r=" + new Date().getTime(); // prevent IE from caching results
    url += "&from_place=" + ((page - 1) * PAGE_SIZE + 1);
    url += "&to_place=" + (page * PAGE_SIZE);
    request.open("GET", url, true);
    request.send(null);
}
