METUI_FUN['$uicss']={
	name:'$uicss',
	init:function(){
	   var widow_width=$(window).width();
	   if(widow_width<=1200){
	   	   var height_nav=$('.$uicss .$uicss-body').width();
		   $('.$uicss .form-group.m-b-0 .btn-primary').css('max-width',height_nav);
		   $('.myfeedback').css('max-width',height_nav);
	   }
	   
	}
};
var x=new metui(METUI_FUN['$uicss']);

