// zouv
// 2017-02-19
// 网络管理，收发包

var NetManager = {
    ws : null,
    Init: function(ip, port, connectCB)
    {
        ip = "120.77.76.233"
        var self = this;
        var ws = new WebSocket('ws://' + ip + ':17301/');
    	ws.onopen = function()
        {
    		console.log("websocket连接成功! ", ip);
            connectCB();
    	};
    	ws.onmessage = function(e)
        {
            self.ReceiveData(event.data);
    	};
    	ws.onclose = function()
        {
    		console.log("websocket连接已断开!");
    	};
    	ws.onerror = function(e)
        {
    		console.log("ERROR:" + e.data);
    	};
        this.ws = ws;
    },
    ReceiveData: function(data)
    {
        if (data instanceof Blob) 
        {
            var blob = data;
            var reader = new FileReader();   // 通过FileReader读取blob数据
            reader.addEventListener("loadend", function() {
                var buffer = new Uint8Array(reader.result);
                var protoId = UtilNet.BytesToInt(buffer, 0, 2);
                var protoName = ProtoManager.GetProtoName(protoId);
                var msgBin = UtilNet.SubBytes(buffer, 2, buffer.length - 2);
                console.log("receive buffer : ", protoId, protoName, msgBin);
                var msg = ProtoManager.Decode(protoName, msgBin);
                ProtoManager.DispatchMsg(protoName, msg);
            });
            reader.readAsArrayBuffer(blob);
        }
        else 
        {
            console.log("ERROR receive data", data);
        }
    },
    SendData: function(protoName, msg)
    {
        console.log("SendData11 : ", msg);
        var buffer = ProtoManager.Encode(protoName, msg);
		console.log("send buffer : ", buffer);
		this.ws.send(buffer);
   	},
}
