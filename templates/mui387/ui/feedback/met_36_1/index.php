<?php defined('IN_MET') or exit('No permission'); ?>
<section class="$uicss animsition" m-id='{$ui.mid}' m-type="nocontent">
    <div class="container">
    	<div class="row">
    		<if value="$ui['contact']">
	    	    <div class="col-md-3 col-xs-12">
	    			<div class="column_inner">
	    				<div class="column_containter">
							<div  class="qfy-element">
								<div class="qfe_wrapper">
									<div class="left-title">
										<div style="font-size: 16px;">
											<strong>
												<span>{$ui.left_text}</span>
											</strong>
										</div>
									</div>
									<div class="contact_us">
										<div>
											<div class="left-content-title">
												<span>{$ui.tel}</span><p>{$ui.tel_text}</p>
											</div>
										</div>
										<div>
											
											<div class="left-content-title">
												<span>{$ui.address}</span><p>{$ui.address_text}</p>
											</div>
										</div>
										<div>
											
											<div class="left-content-title">
												<span>{$ui.qq}</span><p>{$ui.qq_text}</p>
											</div>
										</div>
										<div>
											
											<div class="left-content-title">
												<span>{$ui.ww}</span><p>{$ui.ww_text}</p>
											</div>
										</div>
										<div>
											
											<div class="left-content-title">
												<span>{$ui.email}</span><p>{$ui.email_text}</p>
											</div>
										</div>
										<div class="ewm">
											<?php $img=strstr($ui['wx_img'],"upload"); ?>
											<if value="$img">
												<img src="{$ui.wx_img}" alt="微信">
											</if>
										</div>
									</div>
								</div> 
							</div> 
		 				</div>
					</div>
				</div>
			</if>
	        <div class="
	        <if value='$ui[contact]'>
	        	col-md-8  col-xs-12 
	        </if> $uicss-body">
	            <div m-id='{$ui.mid}' m-type='feedback' class="myfeedback">
	                <tag action='feedback.form'></tag>
	            </div>
	        </div>
    	</div>
    </div>
</section>