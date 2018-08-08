<template>

    <div v-show="connectStatus.show" class="connect" flex="main:center cross:center">

      <div class="modal" flex="main:center cross:center">
        {{connectStatus.msg}}
      </div>
    </div>
</template>


<script>
  import {mapGetters} from 'vuex'
    export default {
        data() {
            return {

            }
        },
        mounted() {

        },
        created() {

        },
        computed: {
          ...mapGetters({
            'connectStatus':'connectStatus'
          })
        }
    }
</script>

<style scoped lang="scss">
.connect{
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: 100000;
  background: rgba(0,0,0,.56);
  .modal{
    background: #012118;
    color: #219555;
    font-size: .28rem;
    width: 3rem;
    height: 3rem;
    border-radius: .3rem;
    box-shadow: 0 0 4px rgba(0,0,0,.5);
  }

}
</style>
