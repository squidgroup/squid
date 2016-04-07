
$(document).ready(function(){ 
  
  // Connect the tooltip contain to each icon 
  $(document).ready(function(){
    $('[data-toggle="tooltip"]').tooltip(); 
	});
  
  
  
  /*$('i.info').each(function() {
  
    var myElement = $(this);
    var id = myElement.attr('id');
    
    myElement.qtip({
      style  : { classes: 'qtip-bootstrap' },
      content: {
          text: 'Loading...', // The text to use whilst the AJAX request is loading
          ajax: {
              type: 'GET', // POST or GET
              url: '../info/'+id+'.html', // URL to the local file              
              dataType: "html",
              success: function(data){
                          
                  // Set the content
                  this.set('content.text', data);
              }
          }
      }
    });
  });*/

  // Resize the input matrix
  function resize(tbody, rowVal, colVal) {
    var template = '<td><div tabindex="0"></div></td>';
    
    var origRows = tbody.children().size();
    var origCols = origRows == 0 ? 0 : tbody.children().first().children().size();
    
    if (colVal === null)
      colVal = origCols;
    
    if (rowVal < origRows) {
      tbody.children().slice(rowVal).remove();
    } else if (rowVal > origRows) {
      while (rowVal-- > origRows) {
        var tr = $('<tr>');
        for (var i = 0; i < colVal; i++) {
          tr.append(template);
        }
        tbody.append(tr);
      }
    }
    
    if (colVal != origCols && origRows != 0) {
      var rowsToModify = tbody.children().slice(0, origRows);
      if (colVal > origCols) {
        while (colVal-- > origCols) {
          rowsToModify.append(template);
        }
      } else {
        rowsToModify.each(function() {
          $(this).children().slice(colVal).remove();
        });
      }
    }
    
    tbody.parent().change();
  }
  
  // listenning the change of the number of traits
  $("#NT").change(function() {
      var dim = $(this).val();
      var tbody = $('#Vind').children('tbody');
      // Resize input matrix dimension when number of traits change
      resize(tbody, dim*6, dim*6);      
  });
  
  
  //$(".tableinput-plusrow, .tableinput-minusrow").remove();
  
  // Environnement div
 /* $(".environnement").hide();
  $("#divRan").show();
  // Show selected environment inputs
  $("#environmentType").change(function() {
      var type = $(this).val();
      switch(type) {
        case "ran":
            $(".environnement").hide();
            $("#divRan").show();
            break;            
        case "lin":
            $(".environnement").hide();
            $("#divLin").show();
            break;            
        case "auto":
          $(".environnement").hide();
          $("#divAuto").show();
          break;          
        default:
            $(".environnement").hide();
      }     
  });
  
  $("#env_preview").change(function() {
    if($(this).is(':checked')){
      $("#environnementPlot").show();
    }else{
      $("#environnementPlot").hide();
    }
  });*/
  
  $(".linkToModuleSteps, #FMod_runButton, #FModSbyS_runButton").click(function() {
     $(window).scrollTop(0);
  });
  
  $(".runningIndicators").hide();
  // Running simulation indicator
  $( ".runButton" ).click(function() {
    $('#isRunning').attr('checked', true);
    $(".runningIndicators").show();
  });
  $("#isRunning").change(function() {
    if($(this).is(':checked')){
      $(".runningIndicators").show();
    }else{
      $(".runningIndicators").hide();
    }
  });
  
});
