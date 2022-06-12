<!doctype html>
<html>
<head>
	<title>Precipitable Water Model</title>
	<link rel="icon" href="assets/img/icon.png">
	<meta charset='utf-8'>
	<meta name="viewport" content="width=device-width, initial-scale=1.0">
	<meta http-equiv="X-UA-Compatible" content="IE=edge,chrome=1">
	<meta name="viewport" content="width=device-width">
	<link rel="preconnect" href="//www.gstatic.com" crossorigin>
	<link rel="preconnect" href="//fonts.gstatic.com" crossorigin>
	<link rel="preconnect" href="//fonts.googleapis.com" crossorigin>
	<link rel="stylesheet" href='assets/css/main.max.css'>
	<link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.6.0/css/all.css">
	<link rel="stylesheet" href="https://fonts.googleapis.com/icon?family=Material+Icons" >
	<link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Source+Code+Pro&display=swap">
	<link rel="stylesheet" href="https://cdn.jsdelivr.net/gh/devicons/devicon@v2.12.0/devicon.min.css">
	<script src="assets/js/jquery.min.js"></script>
	<script>
		$("#content").load("assets/external/head.html");
	</script>
	<script type="text/javascript">var fname = "RESEARCH.md"</script>
	<script type="text/javascript">var bname = "docs"</script>
	<script src="assets/js/header.js"></script>
	<div id="content"></div>
</head>
<body role='flatdoc'>
<div class="demo-layout mdl-layout mdl-js-layout mdl-layout--fixed-drawer mdl-layout--fixed-header">
	<header class="demo-header mdl-layout__header mdl-color--grey-100 mdl-color-text--grey-600" id="top-bar"></header>
	<div id="maintenance"></div>
	<div class="demo-drawer mdl-layout__drawer mdl-color--blue-grey-900 mdl-color-text--blue-grey-50" id="side-nav"></div>
	<main class="mdl-layout__content">
		<div id="modal-maintainers"></div>
		<div id="modal-introduction"></div>
		<div class="menubar" style="padding-right: -100%;"></div>
		<div class='content'>
			<a id="top"></a>
			<div class="collapsible" id="papers">
				<div class="collapsible-header">
					<h2>Papers</h2>
				</div>
				{% for papers in site.data.research.papers %}
				<div class="panel">
					<div class="collapsible_1">
						<div class="panel">
							<h2 style="text-align: center; font-size: 15px">{{papers.title}}</h2>
							<b style="font-weight: bold">{{papers.author}}</b>
							<br>
							<i>{{papers.journal}}</i>
							<br>
							<b>{{papers.status}}</b>
							<br><br>
							<div style="display: flex">
								<a class="button" target="_blank" style="width: 100%; text-align: center" href="https://doi.org/{{papers.doi}}">Web View</a>
								<a class="button" target="_blank" style="width: 100%; text-align: center" href="{{papers.pdf}}">PDF View</a>
							</div>
						</div>
					</div>
				</div>
				{% endfor %}
			</div>
			<div class="collapsible" id="posters">
				<div class="collapsible-header">
					<h2>Posters</h2>
				</div>
				<div class="panel">
					<div class="collapsible_1">
						<div class="panel">
							<img src="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/poster/ams100.png?raw=true" width="100%">
							<h2 style="text-align: center; font-size: 15px">Atmospheric Precipitable Water and its Correlation with Clear Sky Infrared Temperature Readings</h2>
							<b>V. Kelsey, S. Riley</b>
							<br>
							<i>American Meteorological Society 100</i>
							<hr>
							<p>Precipitable water is primarily measured using radiosondes, ground-based global positioning systems (GPS), sun photometers, and microwave radiometry (MWRI). This limits the number of precipitable water measurement sites, which affects forecast accuracy in regards to storm formation, strength, and the potential for precipitation. Socorro, NM is among the sites that do not have the capability to measure precipitable water. Our research builds upon a previous study which determined an exponential relationship between infrared clear sky temperature measurements and precipitable water over the Gulf Coast of Texas. We are analyzing this relationship for the climate zone found in Socorro, NM. Daily ground and clear sky temperature measurements are being taken with low-cost infrared thermometers. Radiosonde precipitable water measurements from Albuquerque and Santa Teresa NWS monitoring sites are input into our dataset and analysed via our newly developed computational tool; which shows that there is a correlation ($R^2 = 0.707$) between clear sky temperature and precipitable water. Our research demonstrates the capability to measure and analyze precipitable water with low cost instrumentation in higher altitude arid climate zones similar to those found in the desert Southwest. We are building a platform to expand our tools and methods so that they can be used to determine and analyse similar correlations over a greater variety of climate zones.</p>
						</div>
					</div>
					<div class="collapsible_1">
						<div class="panel">
							<img src="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/poster/vicki_physcon2019.png?raw=true" width="100%">
							<h2 style="text-align: center; font-size: 15px">Atmospheric Precipitable Water and its Correlation with Clear Sky Infrared Temperature Readings: Field Observations</h2>
							<b>V. Kelsey, S. Riley</b>
							<br>
							<i>Physics Congress 2019</i>
							<hr>
							<p>Precipitable water is the total amount of water vapor which is contained in a vertical column of air that stretches from the Earth’s surface to the top of the atmosphere. It is expressed in terms of what the depth of liquid water would measure once all the water vapor in the column is compressed down into liquid form. Meteorologists currently use ground-based global positioning systems (GPS), microwave radiometers (MWRI), and radiosondes to measure precipitable water. Due to the cost and complexity of these instruments, the number of locations where these measurements are taken is limited; therefore most National Weather Service (NWS) monitoring sites do not measure precipitable water. Meteorologists need precipitable water measurements to help accurately forecast storm formation, strength, and the likelihood of precipitation. We utilize the methodology from a previous study in which relatively low-cost infrared thermometers were used to determine the precipitable water over the Gulf Coast of Texas. This approach is being tested in the climate zone found in Socorro, NM using daily ground and clear sky temperature measurements. This research demonstrates the ability to measure precipitable water with low-cost tools in higher altitude arid climate zones similar to that found in the desert Southwest.</p>
						</div>
					</div>
					<div class="collapsible_1">
						<div class="panel">
							<img src="https://github.com/physicsgoddess1972/Precipitable-Water-Model/blob/docs/docs/assets/img/poster/spencer_physcon2019.png?raw=true" width="100%">
							<h2 style="text-align: center; font-size: 15px">Atmospheric Precipitable Water and its Correlation with Clear Sky Infrared Temperature Readings: Data Analysis</h2>
							<b>S. Riley, V. Kelsey</b>
							<br>
							<i>Physics Congress 2019</i>
							<hr>
							<p>Precipitable water can be defined as the total amount of water vapor that exists in a vertical column of air, traditionally measured via radiosondes. Global Positioning System (GPS) networks and microwave-infrared radiometers can also be used to measure precipitable water by analyzing signal delay; these methods are used by NOAA. Precipitable water measurements can be used to forecast extreme weather events and the potential for precipitation. Based on a previous that analyzed the relationship between precipitable water and zenith sky temperature using infrared thermometers, we have developed a rigorous computational utility to study this same correlation for the Socorro, NM climate system. This research highlights the impact of using low-cost instrumentation to accurately forecast precipitation in regions where the data is not available. After thirty-five weeks, the results of our analysis show an exponential correlation ($R^2 = 0.707$) between precipitable water and clear sky temperature, similar to the trends found in previous studies. However, as we continue to collect data, the intrinsic properties of the correlation are continuously evolving. With the availability of our computational tool, we aim to widen our data to include a diverse set of climate systems to further study the relationship between clear sky temperature and precipitable water while also continuing to collect data for the Socorro, NM area.</p>
						</div>
					</div>
				</div>
			</div>
		</div>
		<div id="bottom-nav"></div>
	</main>
</div>
<script src="assets/js/material.min.js"></script>
</body>
</html>
