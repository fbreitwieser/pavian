$(".tooltip").tooltip({disabled:true});
$(document).click(function() {
    if($(".tooltip").tooltip("option", "disabled")) {
        $(".tooltip").tooltip("enable");
        $(".tooltip").tooltip("open");
        $(".tooltip").off("mouseleave focusout");
    } else {
        $(".tooltip").tooltip("close");
        $(".tooltip").tooltip("disable");
    }
});
