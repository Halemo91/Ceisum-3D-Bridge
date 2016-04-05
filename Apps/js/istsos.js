/**
 * Created by Intern on 10.04.2015.
 */

function getRequestType(browser) {
    switch (browser)
    {
        case "getCapabilities":
            document.getElementById("requestType").innerHTML = browser;
            break;
        case "getObservation":
            document.getElementById("requestType").innerHTML = browser;
            break;
        case "describeSensor":
            document.getElementById("requestType").innerHTML = browser;
            break;
    }
}

// Download a file form a url
function saveFile() {
    //get url from the corresponding DOM element
    var url = document.getElementById("url").innerHTML;
    //create file name
    //to get the text of the response Format selection options: document.getElementById("format").options.[document.getElementById("format").selectedIndex].text
    var filename = document.getElementById("node").value + "." + document.getElementById("format").options[document.getElementById("format").selectedIndex].text
    var xhr = new XMLHttpRequest();
    xhr.responseType = 'blob';
    xhr.onload = function() {
        var a = document.createElement('a');
        a.href = window.URL.createObjectURL(xhr.response); // xhr.response is a blob
        a.download = filename; // Set the file name.
        a.style.display = 'none';
        document.body.appendChild(a);
        a.click();
        delete a;
    };
    xhr.open('GET', url);
    xhr.send();
}
//Ceren add data for Plot
function plotQuery() {
    var url, offering, obsProperties, servURL
    //var request = document.getElementById("requestType").innerHTML;
    var request = "getObservation";
    // var responseFormat = "&responseFormat="+document.getElementById("format").value
    var responseFormat = "&responseFormat=text/x-json";
    var procedure = "procedure=" + document.getElementById("node").value

    //build up the rest standard parts of the request url
    servURL = "http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?service=SOS&request=GetObservation&version=1.0.0&"
    var temperature = document.getElementById("temp").value
    /* var timeBegin = "&eventTime=" + document.getElementById("startDate").value + "+01"
    var timeEnd = "/" + document.getElementById("endDate").value + "+01" */
var timeBegin = "&eventTime=" + document.getElementById("startDate").value.substring(0,10) + "T" + document.getElementById("startDate").value.substring(11,18) + "+01"
				var timeEnd = "/" + document.getElementById("endDate").value.substring(0,10) + "T" + document.getElementById("startDate").value.substring(11,18) + "+01"
    //detect the sensor for which we need data
    var sensor = document.getElementById("node").value
    //build url path according to different sensor cases
    if (sensor == "Node_383" || sensor == "Node_384") {
        offering = "offering=temporary&"
        obsProperties = "&observedProperty=acceleration-x,acceleration-y,acceleration-z"
        url = servURL.concat(offering, procedure, obsProperties, temperature, responseFormat, timeBegin, timeEnd)
    }
    else if (sensor == "displ_node_574") {
        offering = "offering=node_574&"
        obsProperties = "&observedProperty=displacement"
        if (temperature == ",temperature") {
            procedure = procedure + ",t_node_574"
        }
        url = servURL.concat(offering, procedure, obsProperties, temperature, responseFormat, timeBegin, timeEnd);
    }
    else {
        obsProperties = "&observedProperty=strain"
        if (sensor.indexOf("node_573") != -1) {
            offering = "offering=node_573&";
            if (temperature == ",temperature") {
                procedure = procedure + ",t_node_573";
            }
        }
        else if (sensor.indexOf("node_574") != -1) {
            offering = "offering=node_574&";
            if (temperature == ",temperature") {
                procedure = procedure + ",t_node_574";
            }
        }
        url = servURL.concat(offering, procedure, obsProperties, temperature, responseFormat, timeBegin, timeEnd);
        console.log(url);
    }
}
function dataQuery() {
    var url,offering, obsProperties,servURL
    var request = document.getElementById("requestType").innerHTML;
    var responseFormat = "&responseFormat="+document.getElementById("format").value
    var procedure = "procedure="+document.getElementById("node").value
    switch (request) {
        case "getCapabilities":
            url = "http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?request=getCapabilities&service=SOS&version=1.0.0"
            break;
        case "getObservation":
            //build up the rest standard parts of the request url
            servURL = "http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?service=SOS&request=GetObservation&version=1.0.0&"
            var temperature = document.getElementById("temp").value
           var timeBegin = "&eventTime=" + document.getElementById("startDate").value.substring(0,10) + "T" + document.getElementById("startDate").value.substring(11,18) + "+01"
				var timeEnd = "/" + document.getElementById("endDate").value.substring(0,10) + "T" + document.getElementById("startDate").value.substring(11,18) + "+01"

            //detect the sensor for which we need data
            var sensor = document.getElementById("node").value
            //build url path according to different sensor cases
            if (sensor == "Node_383" || sensor == "Node_384") {
                offering = "offering=temporary&"
                obsProperties = "&observedProperty=acceleration-x,acceleration-y,acceleration-z"
                url = servURL.concat(offering,procedure,obsProperties,temperature,responseFormat,timeBegin,timeEnd)
            }
            else if (sensor == "displ_node_574") {
                offering = "offering=node_574&"
                obsProperties = "&observedProperty=displacement"
                if (temperature == ",temperature"){
                    procedure = procedure + ",t_node_574"
                }
                url = servURL.concat(offering,procedure,obsProperties,temperature,responseFormat,timeBegin,timeEnd);
            }
            else {
                obsProperties = "&observedProperty=strain"
                if (sensor.indexOf("node_573") != -1) {
                    offering = "offering=node_573&";
                    if (temperature == ",temperature") {
                        procedure = procedure + ",t_node_573";
                    }
                }
                else if (sensor.indexOf("node_574") != -1) {
                    offering = "offering=node_574&";
                    if (temperature == ",temperature") {
                        procedure = procedure + ",t_node_574";
                    }
                }
                url = servURL.concat(offering,procedure,obsProperties,temperature,responseFormat,timeBegin,timeEnd);
            }
            break;
        case "describeSensor":
            //url = "http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?request=describeSensor&service=SOS&version=1.0.0&procedure=Node_383&outputFormat=text/xml%3Bsubtype%3D%22sensorML/1.0.1%22";
            servURL = "http://quader.igg.tu-berlin.de/istsos/bridgedemoservice?request=describeSensor&service=SOS&version=1.0.0&";
            var outputFormat =  "&outputFormat=text/xml%3Bsubtype%3D%22sensorML/1.0.1%22";
            url = servURL.concat(procedure,outputFormat);
            break;
    }


    //pass url to html element a
    document.getElementById("url").innerHTML = url;

    //create an XMLHttpRequest object
    var xmlhttp = new XMLHttpRequest();

    //set up the callback function
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState == 4 && xmlhttp.status == 200) {
            //if i want JSON array: var myArr = JSON.parse(xmlhttp.responseText);
            var xml = xmlhttp.response;
            var xml_stripped = xml.replace(/\</g,"&lt;")
            xml_stripped = xml_stripped.replace(/\>/g,"&gt;")
            //document.getElementById("myDiv").innerHTML = xml_stripped;
            window.open(url, '_blank');
        }
        else if (xmlhttp.status == 400) {
            alert("XML could not be found")
        }
    }

    //*****************************************
    //		SEND REQUEST TO SERVER
    //*****************************************
    /*Parameters:
     GET: Request method
     url: location of file on the server
     true: asynchronous process
     */
    xmlhttp.open("GET",
        url,
        true);
    //Send request to server
    xmlhttp.send(null);

}