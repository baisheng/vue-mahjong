<template>
  <div v-show="showroomtime.show">
    <div class="time" >
      <div class="list">
        <a href="javascript:;" :class="{'item':true, active:time==15?true:false}" @click="time=15">15分钟</a>
        <!--<a href="javascript:;" :class="{'item':true, active:time==20?true:false}" @click="time=20">20分钟</a>-->
        <a href="javascript:;" :class="{'item':true, active:time==30?true:false}" @click="time=30">30分钟</a>
        <a href="javascript:;" :class="{'item':true, active:time==60?true:false}" @click="time=60">60分钟</a>
      </div>
      <a href="javascript:;" class="sure" @click="openRoom">确定</a>
    </div>
    <div class="bg" @click="hideTime"></div>
  </div>
</template>


<script>
  import {mapGetters} from 'vuex'
  import  brg from '../../utils/middle'

  export default {
    data() {
      return {
        time: 15
      }
    },
    mounted() {


    },
    created() {

    },
    methods: {
      hideTime() {
        this.$store.dispatch('showEnterRoomTime', {show: false})
      },
      openRoom() {
        let timeM = this.time*10


        this.socket.send(201,{
          roomId:0,
          type:this.showroomtime.type,
          longTime:parseInt(this.time)*60
        })
        this.$store.dispatch('showEnterRoomTime', {show: false,time:this.time})

      }
    },
    computed: {
      ...mapGetters({
        'showroomtime': 'showroomtime',
        'room_id': 'room_id',
      })
    }
  }
</script>

<style scoped lang="scss">
  .bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 10000;
    background: rgba(0, 0, 0, .8);
  }

  .time {
    text-align: center;
    position: fixed;
    top: 40%;
    left: 50%;
    width: 80%;
    z-index:100001;
    -webkit-transform: translate(-50%, -50%);
    -moz-transform: translate(-50%, -50%);
    -ms-transform: translate(-50%, -50%);
    -o-transform: translate(-50%, -50%);
    transform: translate(-50%, -50%);
    padding: .5rem;
    border: 1px solid #000;
    background: #ffe8bc;
    border-radius: .15rem;

    .list {
      &:after {
        content: '';
        display: block;
        clear: both;
      }
    }

    .item {
      display: inline-block;
      width: 40%;
      float: left;
      text-decoration: none;
      color: #fff;
      font-size: .35rem;
      padding: .2rem;
      margin: 5%;
      background: #4c0a0c;
      border-radius: .1rem;
      box-shadow: 4px 4px 10px rgba(0, 0, 0, .3);
      &.active {
        color: #fff;
        background: #e60012;
      }
    }
    .sure {
      margin: .8rem auto;
      margin-bottom: 0;
      display: inline-block;
      text-decoration: none;
      padding: .1rem .4rem;
      background: #e60012;
      color: #fff;
      box-shadow: 0 0 10px rgba(0, 0, 0, .3);
      border-radius: .2rem;

    }

  }
</style>
