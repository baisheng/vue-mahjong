<template>

  <div class="info" flex="cross:center box:first">
    房间号: {{name}}&nbsp;&nbsp;&nbsp;&nbsp;底分: 1 <span class="site">{{fengCircleArr[fengCircle]}}</span> <span
    class="time">{{leftTime}}</span>
    <div class="icon_list"  flex="main:center cross:center box:justify">
      <span class="icon" @click="showZhanji"><img class="icon-20" src="../../../assets/new/order.png" alt=""></span>
      <span class="icon" @click="showRmsg"><img class="icon-20" src="../../../assets/new/message.png" alt=""></span>
    </div>
  </div>

</template>

<script>
  import brg from '../../../utils/middle'
  import {mapGetters} from 'vuex'

  const moment = require('moment')
  export default {
    name: 'info',
    props: ['name'],
    data() {
      return {
        fengCircle: 1,
        endTime:'',
        timer2: null,
        fengCircleArr: ['准备中', '东风圈', '南风圈', '西风圈', '北风圈']
      }
    },
    created(){

      this.endTime = parseInt(this.room_first.longTime)/60+':00'
      if(this.room_status2.endTime){
        this.timer2 && clearInterval(this.timer2)
        this.timer2 = setInterval(() => {

          if(this.room_status2.endTime - moment().unix()<0){

            clearInterval(this.timer2)
            this.$nextTick(() => {
              this.endTime = '00:00'
            })

          }else{

            this.$nextTick(() => {
              this.endTime = (this.room_status2.endTime - moment().unix()) * 1000
            })

          }

        }, 1000)
      }

    },
    mounted() {


      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        if (id == 308) {
          let curentTime = ((new Date()).valueOf())/1000
          if (msg.endTime>curentTime) {
            this.timer2 && clearInterval(this.timer2)
            this.timer2 = setInterval(() => {

              if(msg.endTime - moment().unix()<0){

                clearInterval(this.timer2)
                this.$nextTick(() => {
                  this.endTime = '00:00'
                })

              }else{

                this.$nextTick(() => {
                  this.endTime = (msg.endTime - moment().unix()) * 1000
                })

              }

            }, 1000)

          }else{
            this.$nextTick(() => {
              this.endTime ='00:00'
            })
          }
          this.$nextTick(() => {
            this.fengCircle = msg.fengCircle
          })
        }


      })
    },
    methods: {
      showZhanji() {
        this.$store.dispatch('showZj', true)
      },
      showRmsg() {
        this.$store.dispatch('showRMsg', true)
      },
    },
    computed: {
      ...mapGetters({
        'room_first':'room_first',
        'room_status2':'room_status2',
      }),
      leftTime() {
        if(this.endTime.length<=7){
          return this.endTime
        }else{
          return moment(this.endTime).format('mm:ss');
        }
      }
    }
  }

</script>

<style scoped lang="scss">

  .info {
    z-index: 1000;
    border: 2px solid #0b4e40;
    position: fixed;
    left: 0;
    top: 80%;
    height: .56rem;
    font-size: .24rem;
    transform-origin: 0 0;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    background: #0e594a;
    color: #1ec5a6;
    width: 7rem;
    border-radius: .15rem;
    box-shadow: 0 0 5px rgba(0, 0, 0, .2);
    padding-left: .2rem;
    .time {
      margin: 0 .2rem;
    }
    .site {
      margin: 0 .2rem;
    }
    .icon_list {
      margin-right: .2rem;
      width: 1rem;
      .icon {
        margin: 0 .1rem;
      }

    }

  }

</style>
