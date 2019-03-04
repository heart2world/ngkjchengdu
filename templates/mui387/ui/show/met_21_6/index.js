METUI_FUN['$uicss'] = {
    name:'$uicss',
    appear:function (){
        var indexappear=$('.$uicss [data-plugin=appear]');
        if(indexappear.length){
            indexappear.scrollFun(function(val){
                val.appearDiy();
            });
        }
        console.log($(".ad-zt>div").length);
        $(".ad-zt>div").each(function(){
            var He = $(this).children('.ad-zt-img').height();
            $(this).children('.ad-zt-txt').css({
                height: He+"px",
                lineHeight:He+"px"
            });
          });
    }
};
var x = new metui(METUI_FUN['$uicss']);
