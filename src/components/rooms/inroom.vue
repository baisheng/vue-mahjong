<template>

 <div v-show="showroom.show">
   <div class="enter-room" >
     <div class="en_room" flex="main:center cross:center dir:top">
       <input type="text" v-model="roomId">
       <a class="en" href="javascript:;" @click="enterRoomById">房号进入</a>

     </div>
   </div>
   <div class="bg" @click="hide">

   </div>
 </div>
</template>


<script>
  import {mapGetters} from 'vuex'
  import brg from '../../utils/middle'

  export default {
    data() {
      return {
        roomId: ''
      }
    },
    mounted() {
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;

        if(id==202){
          this.hide()
        }
        //查找房间返回
      })



    },
    created() {

    },
    computed: {
      ...mapGetters({
        'showroom': 'showroom',
      })
    },
    methods: {
      enterRoomById() {
        if (this.roomId == '') {
          alert('输入为空')
        } else {
          this.socket.send(201, {
            roomId: this.roomId,
            type: 123,
            longtime: 15
          })
          brg.$emit('curRoomId', this.roomId)

        }

      },
      hide(){
        this.$store.dispatch('showEnterRoom', {show: false})
      },

    }
  }
</script>

<style scoped lang="scss">
  .bg{
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0,0,0,.8);
    z-index: 9999;
  }
  .en_room {
    position: fixed;
    top: 2rem;
    z-index: 10000;
    left: 50%;
    -webkit-transform: translate(-50%, 0);
    -moz-transform: translate(-50%, 0);
    -ms-transform: translate(-50%, 0);
    -o-transform: translate(-50%, 0);
    transform: translate(-50%, 0);
    width: 5.47rem;
    background: #ffe8bc;
    border: 3px solid #500047;
    border-radius: 8px;
    box-shadow: 3px 3px 5px rgba(0, 0, 0, .2) inset;
    padding: 1rem .2rem;
    overflow: visible;
    padding-bottom: .5rem;
    input {
      display: inline-block;
      width: 4.75rem;
      height: 1.74rem;
      background: #6a3906;
      color: #fff;
      border: 2px solid #cfa972;
      padding-left: 20px;
      box-shadow: 3px 3px 5px rgba(0, 0, 0, .2) inset;
      border-radius: 8px;
      font-size: 1rem;

    }
    .close {
      position: absolute;
      top: -.4rem;
      right: -.4rem;
      width: .8rem;
      height: .8rem;
      img {
        width: 100%;
        height: 100%;

      }
    }

    .en {
      margin-top: .5rem;
      text-decoration: none;
      color: #fff;
      text-align: center;
      display: inline-block;
      padding: .1rem .2rem;
      background: #e60012;
      border-radius: 8px;
      border: none;
      box-shadow: 0 0 5px rgba(0, 0, 0, .3);
    }

  }
</style>
