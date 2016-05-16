var datasets = {};
var dragOver = function(e) { e.preventDefault(); };
var dropData = function(e) {
    e.preventDefault();
    handleDrop(e.dataTransfer.files);
};
var handleDrop = function(files) {
    for (var i = 0, f; f = files[i]; i++) {
    var reader = new FileReader();

    reader.onload = (function(file) {
        return function(e) {
        datasets[file.name.toLowerCase()] = e.target.result;
        Shiny.onInputChange("mydata", datasets);
        var div = document.createElement("div");
        var src = "https://cdn0.iconfinder.com/data/icons/office/512/e42-512.png";
        div.id = "datasets";
        div.innerHTML = [
            "<img class='thumb' src='", src, "' title='", encodeURI(file.name),
            "'/>", "<br>", file.name, "<br>"].join('');
        document.getElementById("drop-area").appendChild(div);
        };
    })(f);
    reader.readAsText(f);
    }
};
// debug
var printData = function(data) {
    var div = document.createElement("div");
    div.innerHTML = datasets[data];
    document.getElementById("data-output").appendChild(div);
};
