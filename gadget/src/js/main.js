function zeroFill(str, len){
  while (str.length < len){
    str = '0' + str;
  }
  
  return str;
}

function rgbToColor(r, g, b){
  return '#' + zeroFill((r).toString(16), 2) + zeroFill((g).toString(16), 2) + zeroFill((b).toString(16), 2)
}

function angleColor(r1, g1, b1, r2, g2, b2, maxValue, value){
  var r = Math.round((r2 - r1) * value / maxValue + r1);
  var g = Math.round((g2 - g1) * value / maxValue + g1);
  var b = Math.round((b2 - b1) * value / maxValue + b1);
  
  return rgbToColor(r, g, b);
}

function valueToStr(value){
  value = parseFloat(value);

  if (isNaN(value)){
    return '?';
  }
  
  var result = '';
  
  if ((value < 1) && (value > -1)){
    result = value.toPrecision(3).toString();
  }
  else if ((value < 10) && (value > -10)){
    result = value.toPrecision(2).toString();
  }
  else
  {
    result = value.toFixed().toString();
  }
  
  if (result.indexOf('.') > 0){
    while (true){
      var ch = result.substring(result.length - 1, result.length);
      
      if ((ch != '0') && (ch != '.')){
        break;
      }
      
      result = result.substring(0, result.length - 1);
      
      if (ch == '.'){
        break;
      }
    }
  }
  
  return result;
}

function colorTemp(value){
  value = parseFloat(value);
  
  if (isNaN(value)){
    return '#888888';
  }
  
  if (value > 40){
    return rgbToColor(255, 50, 0);
  }
  else if (value < -25){
    return rgbToColor(0, 50, 255);
  }
  
  return angleColor(0, 50, 255, 255, 50, 0, 65, value + 25);
}

var gadget = {
  api_key: '65D.H4I7FoN/g',
  uuid:    '',
  sensors: [],

  is: function(){
    return ((typeof(System) != 'undefined') && (typeof(System.Gadget) != 'undefined'));
  },
  
  uuid_new: function(){
    var define = '0123456789abcdef';
    var result = '';
    
    while (result.length < 32){
      var idx = Math.round(Math.random() * 15);
      result += define.substring(idx, idx + 1);
    }
    
    return result;
  },
  
  version: function(){
    if (gadget.is()){
      return System.Gadget.version;
    }
    else{
      return '0.0';
    }
  },
  
  config: {
    get: function(name, def){
      var value = null;
      
      if (gadget.is()){
        value = System.Gadget.Settings.read(name);
      }
      else if (typeof(localStorage) != 'undefined'){
        value = localStorage.getItem(name);
      }
      
      if (!value)
      {
        return def;
      }
      
      return value;
    },
    
    set: function(name, value){
      if (gadget.is()){
        System.Gadget.Settings.write(name, value);
      }
      else if (typeof(localStorage) != 'undefined'){
        localStorage.setItem(name, value);
      }
    }
  },
  
  request: {
    json: function(data, callback){
      var xmlHttp;
    
      try {
        xmlHttp = new ActiveXObject("Msxml2.XMLHTTP");
      }
      catch (e){
        try {
          xmlHttp = new ActiveXObject("Microsoft.XMLHTTP");
        }
        catch (E){
          xmlHttp = false;
        }
      }
      
      if (!xmlHttp && (typeof(XMLHttpRequest) != 'undefined')){
        xmlHttp = new XMLHttpRequest();
      }
      
      if (typeof(callback) != 'undefined'){
        xmlHttp.onreadystatechange = function(){
          if ((xmlHttp.readyState == 4) && (xmlHttp.status == 200)){
            callback(JSON.parse(xmlHttp.responseText));
          }
        }
      }
      
      xmlHttp.open('POST', 'http://narodmon.ru/client.php', true);
      xmlHttp.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
      xmlHttp.send(JSON.stringify(data));
    },

    version: function(callback){
      var data = {
        cmd:     'version',
        uuid:    gadget.uuid,
        api_key: gadget.api_key,
        version: gadget.version()
      }
      
      gadget.request.json(data, callback);
    },
    
    updateList: function(radius, callback){
      var data = {
        cmd:     'sensorList',
        uuid:    gadget.uuid,
        api_key: gadget.api_key,
        radius:  radius
      }
      
      gadget.request.json(data, callback);
    },
    
    updateInfoSubscribe: function(sensor, callback){
      var data = {
        cmd:     'sensorInfo',
        uuid:    gadget.uuid,
        api_key: gadget.api_key,
        sensor:  sensor
      }
      
      gadget.request.json(data, callback);
    }
  },

  repaint: function(){
    var content = $('#content');
    
    content.text('');
    
    $('<a>')
      .addClass('header')
      .attr('href', 'http://narodmon.ru')
      .text('narodmon.ru')
      .appendTo(content);
    
    if (gadget.sensors.length > 0){
      for (var i in gadget.sensors){
        var color = rgbToColor(136, 136, 136);
        var value = valueToStr(gadget.sensors[i].value);
        var ed    = '?';
        
        switch (gadget.sensors[i].type){
          case 1:{
            color = colorTemp(gadget.sensors[i].value);
            ed    = '°C';
          } break;
          case 2:{
            color = rgbToColor(0, 50, 150);
            ed    = '%';
          } break;
          case 3:{
            color = rgbToColor(90, 90, 90);
            ed    = 'мм';
          } break;
          case 4:{
            color = rgbToColor(50, 150, 200);
            ed    = 'м/с';
          } break;
          case 5:{
            color = rgbToColor(0, 100, 0);
            ed    = '°';
          } break;
          case 11:{
            color = rgbToColor(150, 100, 0);
            ed    = 'Вт';
          } break;
        }
        
        var _sensor = $('<a>')
          .addClass('sensor')
          .css('background-color', color)
          .attr('href', 'http://narodmon.ru?id=' + gadget.sensors[i].devId)
          .text(value)
          .appendTo(content);
        $('<span>')
          .addClass('ed')
          .text(ed)
          .appendTo(_sensor);
      }
    }
    
    $('<div>')
      .css('clear', 'both')
      .appendTo(content);

    $('body')
      .css('height', content.height() + 'px');
  },

  timer: function(){
    var sensors = [];
    
    for (var i in gadget.sensors){
      sensors.push(gadget.sensors[i].id);
    }

    gadget.request.updateInfoSubscribe(sensors, function(data){
      for (var i in data.sensors){
        for (var j in gadget.sensors){
          if (gadget.sensors[j].id == data.sensors[i].id){
            gadget.sensors[j].value = data.sensors[i].value;
            gadget.sensors[j].time  = data.sensors[i].time;
            
            break;
          }
        }
      }
      
      gadget.repaint();
    });
  },
  
  update: function(data){
    //{"devices":[{"id":387,"my":0,"name":"khome climate","location":"Нижний Новгород, Нижегородская обл., Россия","distance":6.85,"time":1364568011,"lat":56.279526,"lng":44.072932,"sensors":[{"id":844,"pub":1,"type":1,"name":"Высокая ул.","value":1.4,"time":1364568011}]}]}
    
    gadget.sensors = [];
    
    var dst = 1000;
    var idx = 0;
    
    for (var i in data.devices){
      if (dst > data.devices[i].distance){
        dst = data.devices[i].distance;
        idx = i;
      }
    }
    
    for (var j in data.devices[idx].sensors){
      var item = {
        devId: data.devices[idx].id,
        id:    data.devices[idx].sensors[j].id,
        type:  data.devices[idx].sensors[j].type,
        value: data.devices[idx].sensors[j].value,
        time:  data.devices[idx].sensors[j].time
      }
      
      gadget.sensors.push(item);
    }

    gadget.repaint();
  },
  
  init: function(){
    gadget.uuid = gadget.config.get('uuid');
    
    if (!gadget.uuid){
      gadget.uuid = gadget.uuid_new();
      
      gadget.config.set('uuid', gadget.uuid);
    }
  },
  
  main: function(){
    gadget.init();
    
    if (gadget.is()){
      System.Gadget.settingsUI       = 'settings.html';
      System.Gadget.onSettingsClosed = function(){
        gadget.repaint();
      };
    }

    gadget.repaint();
    gadget.request.updateList(1000, gadget.update);
    gadget.request.version();
    
    gadget.timer();
    
    setInterval(gadget.timer, 1000 * 60 * 5);
  },
  
  setting: function(){
    gadget.init();
  }
}