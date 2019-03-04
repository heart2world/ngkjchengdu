METUI_FUN['$uicss']={
	name:'$uicss',
	init:function(){
		var indexappear=$('.met-index-body:eq(0) [data-plugin=appear]');
        if(indexappear.length){
            indexappear.scrollFun(function(val){
                val.appearDiy();
            });
        }

	}
};
var x=new metui(METUI_FUN['$uicss']);