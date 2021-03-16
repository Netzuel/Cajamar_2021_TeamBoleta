// !preview r2d3 data=list(toJSON(lol2, na="null"), color_book), d3_version=6
//
// r2d3: https://rstudio.github.io/r2d3
//

var margin = {left: width/60, right: width/60, top: height/12, bottom: height/6};

var tooltip = d3.select("body").append("div")
        .attr("class", "tooltip")
        .style("opacity", 0)
        .style("position", "absolute")
        .style("background", "#3EB668")
        .style("padding", "2px 4px 2px 4px")
        .style("border-radius", "2px")
        .style("font-family", "Rajdhani")
        .style("font-size", "14px")
        .style("font-weight", 500)
        .style("color", "#fff")
        .style("text-align", "center")
        .style("pointer-events", "none");
        
svg.append("text")
    .attr("x", 10)
    .attr("y", 20)
    .attr("text-anchor", "start")
    .attr("fill", "black")
    .text("Volumen comercializado en MercaMadrid")
    .style("font-family", "Rajdhani")
    .style("font-size", "16px")
    .style("font-weight", "700");


r2d3.onRender(function(data, svg, width, height, options) {
  svg.attr("viewBox", `0 0 ${width} ${height}`)
       .attr("preserveAspectRatio", "xMidYMid meet");
       
  var streamData = data[0],
    colorBook = data[1];
  
  var keys = Object.keys(streamData[0]).slice(1);
  
  streamData.forEach(e => {
    e.date = d3.timeParse("%Y-%m-%d")(e.date)
  });
  
  var series = d3.stack()
    .keys(keys)
    .offset(d3.stackOffsetWiggle)
    .order(d3.stackOrderInsideOut)(streamData);
  
  var x = d3.scaleUtc()
    .domain(d3.extent(streamData, d => d.date))
    .range([margin.left, width-margin.right])
  
  var y = d3.scaleLinear()
    .domain([d3.min(series, d => d3.min(d, d => d[0])), d3.max(series, d => d3.max(d, d => d[1]))])
    .range([height-margin.bottom, margin.top]);
  
  var area = d3.area()
    .x(d => x(d.data.date))
    .y0(d => y(d[0]))
    .y1(d => y(d[1]))
    .curve(d3.curveMonotoneX);
    
  svg.append("line")
    .attr("x1", x(new Date("2020-03-15")))
    .attr("y1", height-margin.bottom/1.5)
    .attr("x2", x(new Date("2020-11-15")))
    .attr("y2", height-margin.bottom/1.5)
    .attr("stroke", "red")
    .attr("opacity", 0.7)
    .attr("stroke-width", 3);
  
  //console.log(x(new Date("2020-11-15"))-x(new Date("2020-03-15"))+height-margin.bottom);
  svg.append("text")
    .attr("x", x(new Date("2020-11-15")))
    .attr("y", height-margin.bottom/2)
    .attr("text-anchor", "end")
    .attr("fill", "red")
    .attr("opacity", 0.7)
    .text("COVID-19 pandemic")
    .style("font-family", "Rajdhani")
    .style("font-size", "12px")
    .style("font-weight", "700");
  
  svg.append("g")
    .selectAll("path")
    .data(series)
    .join("path")
      .attr("fill", ({key}) => colorBook[key])
      .attr("d", area)
      .on("mouseover", function(event, d) {
        tooltip.transition()
          .duration(500)
          .style("opacity", 0.9);
        tooltip.html(d.key);
      })
      .on("mousemove", function(event, d) {
        tooltip
          .style("left", (event.pageX+20)+"px")
          .style("top", (event.pageY-20)+"px")
          .style("border", "2px solid "+colorBook[d.key]);
      })
      .on("mouseout", function(event, d) {
        tooltip.transition()
          .duration(500)
          .style("opacity", 0);
      })
      .on("click", function(event, d) {
        //console.log(d3.select(this).attr("name"));
        Shiny.setInputValue(
          "mercas_streamgraph_click",
          d.key,
          {priority: "event"}
        );
      });
  
  var dateAxisGen = d3.axisBottom()
                    .scale(x);
  var dateAxis = svg.append("g")
    .attr("transform", "translate("+0+","+(height-margin.bottom)+")")
    .call(dateAxisGen);
    
  dateAxis
    .selectAll("text")
    .style("font-size", "12px")
    .style("font-weight", 500)
    .style("font-family", "Rajdhani");
  
  dateAxis
    .selectAll("line")
    .remove();
  dateAxis
    .selectAll(".domain")
    .remove();
    
  var legend = svg.append("g");

  legend
      .append("text")
      .attr("x", 30)
      .attr("y", 40+(y(150000000.0)-y(160000000.0))/2)
      .attr("text-anchor", "start")
      .attr("alignment-baseline", "middle")
      .attr("fill", "black")
      .text("10k t")
      .style("font-family", "Rajdhani")
      .style("font-size", "12px")
      .style("font-weight", "700");
      
  legend.append("line")
    .attr("x1", 20)
    .attr("y1", 40)
    .attr("x2", 20)
    .attr("y2", 40+y(150000000.0)-y(160000000.0))
    .attr("stroke", "black")
    .attr("stroke-width", 2);
});

r2d3.onResize(function (width, height) {

});


