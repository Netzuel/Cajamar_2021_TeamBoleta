// !preview r2d3 data=list(spain.topojson, madrid.point, toJSON(madrid.cherry), title), dependencies = "topojson.min.js", d3_version=6
//
// r2d3: https://rstudio.github.io/r2d3
//

//Extrayendo datos, generando proyección, etc
var spain = topojson.feature(data[0], data[0].objects.foo),
    madrid = data[1],
    projection = d3.geoMercator()
      .scale(1100)
      .center([-13, 46])
      .translate([0, 0]),
    path = d3.geoPath(projection);
      
// Conversión de algunos tipos mal parseados
madrid.X = parseFloat(madrid.X);
madrid.Y = parseFloat(madrid.Y);

var curveLines = d3.line().curve(d3.curveCardinal)

//tooltip de las provs
var tooltip = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0)
    .style("position", "absolute")
    .style("background", "#1D8F45")
    .style("padding", "2px 4px 2px 4px")
    .style("border-radius", "2px")
    .style("font-family", "Rajdhani")
    .style("font-size", "14px")
    .style("font-weight", 500)
    .style("color", "#fff");
    
// Mapa de españa
svg.selectAll("#spain")
  .data(spain.features)
  .join("path")
  .attr("class", "spain")
  .attr("d", f => path(f))
  .attr("fill", "#2AC360")
  .attr("stroke", "#2AC360")
  .attr("stroke-width", 2)
  .attr("stroke-miterlimit", 1);

//Función para formatear cantidades (en k, M, etc)
function formatVolumen(volumen, ud, decs=2) {
  if (volumen < 1e3) {
    return volumen.toFixed(decs)+ud
  } else if (volumen < 1e6) {
    if (ud != "kg") {
      ud = "k"+ud
    } else {
      ud = "t"
    }
    return (volumen/1e3).toFixed(decs)+ud
  } else {
    
    if (ud == "kg") {
      volumen /= 1e3
      ud = "t"
    } else {
      volumen /= 1e6
      ud = "M"+ud
    }
    return volumen.toFixed(decs)+ud
  }
}

r2d3.onRender(function(data, svg, width, height, options){
  svg.selectAll("text")
      .remove();
  svg.selectAll(".conection")
      .remove();
  svg.selectAll(".prov")
      .remove();
  svg.selectAll(".madrid")
      .remove();
  
  var productData = data[2],
      title = data[3];
      
  productData.forEach(e => {
    e.X = parseFloat(e.X);
    e.Y = parseFloat(e.Y);
  });
  
  svg.append("text")
        .attr("x", (width / 30))             
        .attr("y", 0 + (20))
        .attr("text-anchor", "left")  
        .style("font-size", "16px")
        .style("font-weight", 500)
        .style("font-family", "Rajdhani")  
        .text(title);

  // Variables necesarias para las conexiones
  var maxVolumen = d3.max(productData, d => d.volumen)
  var colorScale = d3.scaleLinear()
      .domain([d3.min(productData, d => d.price_mean), d3.max(productData, d => d.price_mean)])
      .range(["red", "blue"]);
  
  // Conexiones
  conecs = svg.selectAll(".conection")
    .data(productData)
    .join("path")
    .attr("class", "conection")
    .attr("d", function(d) {
      //console.log(parseFloat(madrid.X)+parseFloat(d.X))
      return curveLines(
        [
          projection([d.X, d.Y]),
          projection([
            (1+0.01*(Math.random()*2-1))*(madrid.X+d.X)/2, 
            (1+0.01*(Math.random()*2-1))*(madrid.Y+d.Y)/2
          ]),
          projection([madrid.X, madrid.Y]), 
        ]
      );
    })
    .attr("stroke", d => colorScale(d.price_mean))
    .attr("stroke-width", function(d, i) { return 1+Math.sqrt(d.volumen/maxVolumen)*7; })
    .attr("stroke-miterlimit", 1)
    .attr("fill", "none")
    .attr("opacity", 1)
    
  var totalLengths = conecs.nodes().map(i => i.getTotalLength())
  
  conecs
    .attr("stroke-dasharray", function(d, i) {return "0,"+totalLengths[i];})
    .transition()
    .duration(1500)
    .ease(d3.easeCubic)
    .attr("stroke-dasharray", function(d, i) {return totalLengths[i]+","+totalLengths[i];});
  
  
  
  // Puntos de las provincias
  svg.selectAll(".prov")
    .data(productData)
    .enter()
    .append("circle")
    .attr("class", "prov")
    .attr("cx", d => projection([d.X, 0])[0])
    .attr("cy", d => projection([0, d.Y])[1])
    .attr("r", "5px")
    .attr("fill", "#fff")
    .attr("stroke", "black")
    .attr("stroke-width", 2)
    .attr("name", d => d.prov)
    .on("mouseover", function(event, d) {
      tooltip.transition()
        .duration(200)
        .style("opacity", 0.9);
      tooltip.html("<b>"+d.prov+"</b><br/>"+
        d.price_min.toFixed(2)+" ← "+d.price_mean.toFixed(2)+" → "+
        d.price_max.toFixed(2)+" €/kg<br/>"+formatVolumen(d.volumen, "kg", 1)+
        " - Valor medio: "+formatVolumen(d.price_mean*d.volumen, "€", 2))
        .style("left", (event.pageX+18)+"px")
        .style("top", (event.pageY-28)+"px");
    })
    .on("mouseout", function(d) {
      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    })
    .on("click", function() {
      Shiny.setInputValue(
        "mercas_prov_clicked",
        d3.select(this).attr("name"),
        {priority: "event"}
      );
    });
    
    // Madrid - punto central
  svg.selectAll(".madrid")
    .data([madrid])
    .enter()
    .append("circle")
    .attr("class", "madrid")
    .attr("cx", d => projection([d.X, 0])[0])
    .attr("cy", d => projection([0, d.Y])[1])
    .attr("r", "8px")
    .attr("fill", "#fff")
    .attr("stroke", "black")
    .attr("stroke-width", 2)
    .attr("opacity", 1)
    .on("mouseover", function(event, d) {
      tooltip.transition()
        .duration(200)
        .style("opacity", 0.9);
      tooltip.html("<b>"+d.prov+"</b>")
        .style("left", (event.pageX)+"px")
        .style("top", (event.pageY-28)+"px");
    })
    .on("mouseout", function(d) {
      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    });

});



//console.log(d3.max(productData, d => d.price_mean))
//console.log(projection([data[3].X, data[3].Y]))
//console.log(madrid)



  